module Run ( SatDict
           , Log
           , runAD
           , runBF
           , runVSMT
           , fst'
           , IncPack
           , smtBoolWith
           , smtInt
           , BValue(..)
           , evaluate
           , toBValue
           , propToSBool
           , emptySt
           , runPonV
           ) where

import           Control.Arrow (first, second)
import           Control.DeepSeq (force)
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict as St
import Control.Monad.IO.Class (MonadIO)
import           Data.Foldable (foldr')
import           Data.HashSet (HashSet, member, insert)
import qualified Data.Map.Strict as M
import qualified Data.SBV as S
import qualified Data.SBV.Control as SC
import qualified Data.SBV.Internals as I
import           Data.Text (unpack, pack, Text)
import           Data.List (intersperse)
import           Prelude hiding (LT, GT, EQ)

import           Data.Maybe (catMaybes)

import           SAT
import           VProp.Types
import           VProp.SBV
import           VProp.Core
import           Utils
import           Config
import           Result

import           Text.Show.Unicode          (ushow)

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- (an mapping from dimensions to booleans denoting selection) and values are
-- whether that config is satisfiable or not (a bool)
type SatDict a = M.Map (Config a) Bool

-- | Type convenience for Log
type Log = Text

-- | Takes a dimension d, a type for values a, and a result r
type Env d = RWST (SMTConf d Text Text) Log (SatDict d) IO

-- | An empty reader monad environment, in the future read these from config
-- file
_emptySt :: SatDict d
_emptySt = M.empty

-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env d r ->
           SMTConf d Text Text ->
           SatDict d ->
           IO (r, SatDict d,  Log)
_runEnv = runRWST


runEnv :: (ReadableProp d -> Env d (Result d)) ->
          SMTConf d Text Text ->
          ReadableProp d ->
          IO (Result d, SatDict d, Log)
runEnv f conf x = _runEnv (f x') conf _emptySt
  where x' = foldr' ($!) x (opts conf)

runAD :: (Show d, Resultable d, SAT (ReadableProp d)) =>
         SMTConf d Text Text
      -> ReadableProp d
      -> (d -> Text)
      -> IO (Result d)
runAD os p f = fst' <$> runEnv (flip runAndDecomp f) os p

runBF :: (Show d, Resultable d, SAT (ReadableProp d)) =>
         ConfigPool d        ->
         SMTConf d Text Text ->
         ReadableProp d      ->
         IO (Result d)
runBF pool os p = fst' <$> runEnv (runBruteForce pool) os p

-- runPonV :: (Show d, Resultable d) => ConfigPool d -> ReadableProp d
--         -> IO (Result d, SatDict d, Log)
runPonV pool prop = runEnv (runPlainOnVSat pool) emptyConf prop

-- | Run the VSMT solver given a list of optimizations and a prop
runVSMT :: (Show d, Resultable d) =>
           ConfigPool d        ->
           SMTConf d Text Text ->
           ReadableProp d      ->
           IO (Result d, SatDict d, Log)
runVSMT = runEnv . runVSMTSolve

-- | Given a VProp a term generate the satisfiability map
initSt :: Ord d => ConfigPool d -> VProp d a a -> SatDict d
initSt []   prop = M.fromList . fmap (\x -> (x, False)) $ choices prop
initSt pool prop = M.fromList $ zip pool (repeat False)

-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter [Char] m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter [Char] m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x

-- | run the sat solver, when we get a satisafiable call get the assignments
-- directly
runForDict :: (Resultable d, SAT (ReadableProp d)) =>
  ReadableProp d -> IO (Result d)
runForDict x = S.runSMT $
  do
    x' <- toPredicate x
    SC.query $
      do S.constrain x'
         res <- getResultWith (toResultProp . LitB)
         SC.resetAssertions
         SC.exit
         return res


-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runBruteForce ::
  (MonadTrans t, Show d, Resultable d , MonadState (SatDict d) (t IO)
  , SAT (ReadableProp d)) =>
  ConfigPool d        ->
  ReadableProp d      ->
  t IO (Result d)
runBruteForce pool prop = lift $ flip evalStateT (initSt pool prop) $
  do
  _confs <- get
  let confs = M.keys _confs
      plainProps = if null confs
        then pure (M.empty, prop)
        else (\y -> (y, selectVariantTotal y prop)) <$> confs
  plainMs <- lift $
             mapM (bitraverse
                   (pure . configToResultProp)
                   runForDict) $ plainProps
  return $ mconcat (fmap (\(c, as) -> insertToSat c as) plainMs)
  where
        helper c as =  insertToSat c as


-- | Run plain terms on vsat, that is, perform selection for each dimension, and
-- then run on the vsat solver. We know that this will always become a Unit, and
-- will always hit solvePlain.
runPlainOnVSat
  :: (Show d, Resultable d, MonadIO m,
      MonadReader (SMTConf d a a) m) =>
     ConfigPool d -> ReadableProp d -> m (Result d)
runPlainOnVSat pool prop = flip evalStateT (initSt pool prop) $
 do
  _confs <- get
  let confs = M.keys _confs
      plainProps = if null confs
        then [Just (M.empty, prop)]
        else (\y -> sequence $! (y, selectVariant y prop)) <$> confs
  plainMs <- mapM (bitraverse
                   (pure . configToResultProp)
                   (runVSMTSolve mempty)) $ catMaybes plainProps
  return $ foldMap (uncurry helper) plainMs
  where
        helper c as =  insertToSat c as

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (Show d, Resultable d, MonadTrans t, Monad (t IO), SAT (ReadableProp d)) =>
  ReadableProp d -> (d -> Text) -> t IO (Result d)
runAndDecomp prop f = lift $ runForDict $ andDecomp prop (f . dimName)

runVSMTSolve ::
  (Show d, MonadIO m, Resultable d , MonadReader (SMTConf d a a) m) =>
  ConfigPool d ->
  ReadableProp d ->
  m (Result d)
runVSMTSolve configPool prop =
  do cnf <- ask
     -- convert all refs to SBools
     let prop' = St.evalStateT (propToSBool prop) (mempty, mempty)
     -- run the inner driver
     -- res <- liftIO $! S.runSMTWith (conf cnf){S.verbose=True} $! vSMTSolve prop' configPool
     res <- liftIO $! S.runSMTWith (conf cnf) $! vSMTSolve prop' configPool
     return $! res

-- | wrapper around map to keep track of the variable references we've seen, a,
-- and their symbolic type, b, which is eta reduced here
type UsedVars a = M.Map a

-- | a set of dimensions that have been processed and have generated models
type GenModel = Bool

-- | A state monad transformer that holds two usedvar maps, one for booleans and
-- one for doubles
type IncPack a = St.StateT (UsedVars a S.SBool, UsedVars a SNum) S.Symbolic

-- | a pool of configurations used to perform selection. A list is appropriate
-- here because we only use it as a stack, or a single traversal and we require
-- the semantics of a possible infinite (co-inductive for you nerds)
-- data structure
type ConfigPool a = [Config a]

type Used = HashSet Text

type UsedConstraint = Text

-- | the internal state for the incremental solve algorithm, it holds a result
-- list, and the used dims map, and is parameterized by the types of dimensions,
-- d
data IncState d =
  IncState { result      :: !(Result d) -- * the result map
           , config      :: !(Config d) -- * the current config
           , processed   :: !GenModel   -- * a flag denoting
                                        -- that a model has been
                                        -- generated during a
                                        -- recursive call
           , usedConstraints :: !(Used) -- * a set that checks constraints so we
                                        -- don't send redundant constraints to
                                        -- sbv
           } deriving (Eq,Show)

emptySt :: (Resultable d) => IncState d
emptySt = IncState{ result=mempty
                  , config=mempty
                  , processed=False
                  , usedConstraints=mempty
                  }

onResult :: (Result d -> Result d) -> IncState d -> IncState d
onResult f IncState {..} = IncState {result=f result, ..}

onConfig :: (Config d -> Config d) -> IncState d -> IncState d
onConfig f IncState {..} = IncState {config=f config, ..}

deleteFromConfig :: Ord d => (Dim d) -> IncState d -> IncState d
deleteFromConfig = onConfig . M.delete

insertToConfig :: Ord d => (Dim d) -> Bool -> IncState d -> IncState d
insertToConfig d b = onConfig $ M.insert d b

replaceConfig :: Config d -> IncState d -> IncState d
replaceConfig = onConfig . const

onProcessed :: (GenModel -> GenModel) -> IncState d -> IncState d
onProcessed f IncState {..} = IncState {processed=f processed, ..}

onUsed :: (Used -> Used) -> IncState d -> IncState d
onUsed f IncState {..} = IncState {usedConstraints =f usedConstraints, ..}

isUsed :: UsedConstraint -> Used -> Bool
isUsed = member

insertUsed :: UsedConstraint -> IncState d -> IncState d
insertUsed = onUsed . insert

-- | the incremental solve monad, with the base monad being the query monad so
-- we can pull out sbv models Hardcoding so that I don't have to write the mtl
-- typeclass. I do not expect these to change much
type IncVSMTSolve d = St.StateT (IncState d) SC.Query

-- | Top level wrapper around engine and monad stack, this sets options for the
-- underlying solver, inspects the results to see if they were variational or
-- not, if not then it gets the plain model
vSMTSolve :: (Show d, Resultable d) =>
  S.Symbolic (VProp d (S.SBool, Name) SNum) ->
  ConfigPool d -> S.Symbolic (Result d)
vSMTSolve prop configPool =
  do prop' <- prop
     S.setOption $ SC.ProduceUnsatCores True
     -- trace ("Solving with configPool: " ++ show configPool) $ return ()
     SC.query $
       do
         -- partially evaluate the prop
         let !pprop = (findPrincipleChoice . mkTop) <$> (evaluate . toBValue $! prop')

         -- now we avoid redundent computation by solving on the evaluated
         -- proposition instead of the input proposition
         (b,resSt) <- St.runStateT (pprop >>= solveVariational configPool) emptySt

         -- check if no models were generated, if that is the case then we had a
         -- plain formula as input. This means that the result of
         -- evaluation/accumulation will be a single symbolic boolean reference
         if isResultNull (result resSt)
           then solvePlain b
           else return $ result resSt

solvePlain :: Resultable d => S.SBool -> SC.Query (Result d)
solvePlain b = do S.constrain b; getResultWith (toResultProp . LitB)

solveVariational :: (Show d, Resultable d) =>
                    ConfigPool d ->
                    Loc d ->
                    IncVSMTSolve d (S.SBool)
solveVariational []        p    = doChoice p
solveVariational cs prop = do mapM_ step cs; return true
  where step cfg = do lift $ SC.push 1
                      -- trace ("Result of Eval: " ++ ushow prop ++ "\n") $ return ()
                      setConfig cfg
                      _ <- doChoice prop
                      lift $ SC.pop 1

-- | The name of a reference
type Name = Text

-- | The name of a constraint is a list of reference names and operators
type ConstraintName = [Name]

-- | This ensures two things: 1st we need all variables to be symbolic before
-- starting query mode. 2nd we cannot allow any duplicates to be called on a
-- string -> symbolic a function or missiles will launch.
propToSBool :: ReadableProp d -> IncPack Text (VProp d (S.SBool, Name) SNum)
propToSBool (RefB x)     = do b <- smtBool x
                              return $! RefB (b, x)
propToSBool (OpB o e)    = OpB  o <$> propToSBool e
propToSBool (OpBB o l r) = OpBB o <$> propToSBool l <*> propToSBool r
propToSBool (ChcB d l r) = ChcB d <$> propToSBool l <*> propToSBool r
propToSBool (OpIB o l r) = OpIB o <$> propToSBool' l <*> propToSBool' r
propToSBool (LitB b)     = return $ LitB b

propToSBool' :: (Ord b, Show b) => VIExpr d b -> IncPack b (VIExpr d SNum)
propToSBool' (Ref RefI i) = Ref RefI <$> smtInt i
propToSBool' (Ref RefD d) = Ref RefD <$> smtDouble d
propToSBool' (OpI o e)    = OpI o    <$> propToSBool' e
propToSBool' (OpII o l r) = OpII o <$> propToSBool' l <*> propToSBool' r
propToSBool' (ChcI d l r) = ChcI d <$> propToSBool' l <*> propToSBool' r
propToSBool' (LitI x)     = return $ LitI x

-- | a builder function that abstracts out the packing algorithm for numbers. It
-- takes a function to convert a string to a symbolic variable like S.sInt64, or
-- S.sDouble, a constructor that add the symbolic type to the sum type SNum and
-- produces a function k -> t m SNum, which reifies to k -> IncPack k SNum.
mkSmt :: (Ord k, MonadTrans t, Monad m, Show k,
           MonadState (d, UsedVars k SNum) (t m)) =>
         (String -> m a) -> (a -> SNum) -> k -> t m SNum
mkSmt f g str = do (_,st) <- get
                   case str `M.lookup` st of
                     Nothing -> do b <- lift . f $ show str
                                   let b' = g b
                                   St.modify' (second $ M.insert str b')
                                   return b'
                     Just x  -> return x


-- | convert every reference to a boolean, keeping track of what you've seen
-- before, can't use mkStatement here because its a special case
smtBoolWith :: (Show a, Ord a) => a -> (a -> String) -> IncPack a S.SBool
smtBoolWith str f =
  do (st,_) <- get
     case str `M.lookup` st of
       Nothing -> do b <- lift . S.sBool $ f str
                     St.modify' (first $ M.insert str b)
                     return b
       Just x  -> return x

smtBool :: Text -> IncPack Text S.SBool
smtBool = flip smtBoolWith unpack

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtInt :: (Show a, Ord a) => a -> IncPack a SNum
smtInt = mkSmt S.sInt64 SI

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtDouble :: (Show a, Ord a) =>  a -> IncPack a SNum
smtDouble = mkSmt S.sDouble SD

-- | check if the current context is sat or not
isSat :: SC.Query Bool
isSat = do cs <- SC.checkSat
           return $ case cs of
                      SC.Sat -> True
                      _      -> False


-- | type class needed to avoid lifting for constraints in the IncSolve monad
instance (Monad m, I.SolverContext m) =>
  I.SolverContext (StateT (IncState d) m) where
  constrain = lift . (force S.constrain)
  namedConstraint = (lift .) . S.namedConstraint
  setOption = lift . S.setOption
  softConstrain = lift . I.softConstrain
  constrainWithAttribute = (lift .) . I.constrainWithAttribute


-- Helper functions for solve routine
store :: Resultable d => Result d -> IncVSMTSolve d ()
store = St.modify' . onResult  . (<>)

setDim :: Ord d => (Dim d) -> Bool -> IncVSMTSolve d ()
setDim = (St.modify' .) . insertToConfig

removeDim :: Ord d => (Dim d) -> IncVSMTSolve d ()
removeDim = St.modify' . deleteFromConfig

setConfig :: Config d -> IncVSMTSolve d ()
setConfig = St.modify' . replaceConfig

hasGenDModel :: IncVSMTSolve d Bool
hasGenDModel = gets processed

setModelGenD :: IncVSMTSolve d ()
setModelGenD = St.modify' $! onProcessed (const True)

setModelNotGenD :: IncVSMTSolve d ()
setModelNotGenD = St.modify' $! onProcessed (const False)

setUsed :: UsedConstraint -> IncVSMTSolve d ()
setUsed = St.modify' . insertUsed

-- | A smart constrain method, this inspects a set of constraint names to see if
-- we have a duplicate constraint name. If we do (if the name representing the
-- sbool is in the set) then we simply constrain the bool unnamed, if not then
-- we add the named constraint and insert into the set. We use an unordered
-- hashset for performance reasons because these strings can get quite long
-- leading to poor Eq performance
constrain :: S.SBool -> ConstraintName -> IncVSMTSolve d ()
constrain b [] = S.constrain b
constrain b !name = do
  used <- gets usedConstraints
  if not (isUsed usedName used)
    then do S.namedConstraint name' b; setUsed usedName
    else S.constrain b
  where !name' = (unpack $ mconcat (intersperse " " name))
        !usedName = mconcat name

toText :: Show a => a -> Text
toText = pack . show

solveVariant :: Resultable d =>
  IncVSMTSolve d I.SBool -> IncVSMTSolve d (Result d)
solveVariant go = do
           setModelNotGenD

           -- the recursive computaton
           lift $! SC.push 1
           _ <- go

           -- check if the config was satisfiable, and if the recursion
           -- generated a model
           bd <- hasGenDModel

           -- if not generated a model, then construct a -- result
           resMap <- if not bd
                     then do prop <- gets (configToResultProp . config)
                             setModelGenD
                             lift $! getResult prop
                      else -- not sat or have gen'd a model so ignore
                       return mempty

           -- reset stack
           lift $! SC.pop 1

           return resMap

handleChc :: (Show d, Resultable d) =>
  IncVSMTSolve d (Result d)
  -> IncVSMTSolve d (Result d)
  -> Dim d
  -> IncVSMTSolve d (Result d)
handleChc goLeft goRight d =
  do currentCfg <- gets config
     case M.lookup d currentCfg of
       Just True  -> goLeft
       Just False -> goRight
       Nothing    -> do
         --------------------- true variant -----------------------------
         setDim d True
         -- trace ("[CHC] Going Left, choice: " ++ show d ++ "\n") $ return ()
         resMapT <- goLeft

         -------------------- false variant -----------------------------
         setDim d False
         -- trace ("[CHC] Going Right, choice: " ++ show d ++ "\n") $ return ()
         resMapF <- goRight

         -- store results and cleanup config
         let !result = resMapT <> resMapF

         -- we remove the Dim from the config so that when the recursion
         -- completes the last false branch of the last choice is not
         -- remembered. For Example if we have A<p,q> /\ B<r,s> then the
         -- recursion starts at A and completes at the right variants of
         -- B, namely s. If we don't remove the dimension from the config
         -- then the config will retain the selection of (B, False) and
         -- therefore on the (A, False) branch of the recursion it'll
         -- miss (B, True)
         removeDim d
         return result

-- | type synonym to simplify the BValue type
type SBVProp d = VProp d (S.SBool, Name) SNum

data BValue d = B !S.SBool !ConstraintName
              | Unit
              | C !(Dim d) !(SBVProp d) !(SBVProp d)
              | BNot !(BValue d)
              | BVOp !(BValue d) !BB_B !(BValue d)

instance Show d => Show (BValue d) where
  show (B _ _) = "B"
  show Unit    = "Unit"
  show (C d _ _) = "C_" ++ show d
  show (BNot e)  = "not (" ++ show e ++ ")"
  show (BVOp l op r) = "\t(" ++ show op ++ "\nLeft:\n\t" ++ show l ++  "\nRight:\n\t " ++ show r ++ ")\n"

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl. This progress in two stages, we extend the value domain with a
-- choice constructor to ensure that choices are evaluated absolutely last. If
-- they are not evaluated last then they will lose information from the rest of
-- the formula. For example if you have: ((c and d) and AA<-a, b>) and a and e,
-- then if you do simple recursion you'll generate a model when you evaluate the
-- choice, which occurs __before__ you evaluate the (a and e), therefore you'll
-- miss a unsatisfiable model. To work around this we evaluate all expression
-- around choices and inside the choices. When we see a choice we pause
-- evaluation and evaluate around and inside the choice, keeping a reference to
-- the result of the choices sibling in the AST. Once we compile down to the
-- value level we evaluate the choices properly by manipulating the assertion
-- stack, resulting in model generation and sbools
toBValue :: Resultable d => VProp d (S.SBool, Name) SNum -> BValue d
toBValue (RefB (b,name)) = B b
toBValue (LitB b) = B (S.literal b)
toBValue (OpB Not (OpB Not notchc)) = toBValue notchc
toBValue (OpB Not e) = driveNotDown e
toBValue (OpBB op (RefB (b,n)) (RefB (b',n'))) = (B bres)
  where bres = (bDispatch op) b b'
toBValue (OpBB op (ChcB d l r) r') = BVOp (C d l r) op (toBValue r')
toBValue (OpBB op l' (ChcB d l r)) = BVOp (toBValue l') op (C d l r)
toBValue (OpBB op l r) = BVOp (toBValue l) op (toBValue r)
toBValue (OpIB _ _ _) = error "Blame Jeff! This isn't implemented yet!"
toBValue (ChcB d l r) = C d l r

-- dbg :: (Show a, Monad m) => String -> a -> m ()
-- dbg s a = trace (s ++ " : " ++ show a ++ " \n") $ return ()

-- | Evaluation allows communication with the solver and reduces terms to Unit
-- values thereby representing that evaluation has taken place. Evaluation can
-- call a switch into the accumulation mode in order to partially evaluate an
-- expression
evaluate :: (Resultable d) => BValue d -> IncVSMTSolve d (BValue d)
evaluate Unit        = return Unit                             -- [Eval-Unit]
evaluate !(B b) =
  -- trace "Eval B" $
  do S.constrain b; return Unit           -- [Eval-Term]
evaluate !(x@(C _ _ _)) = return x                             -- [Eval-Chc]
-- evaluate (BVOp Unit _ Unit) = trace "double unit" $ return Unit                     -- [Eval-UAndR]
evaluate !(BVOp Unit _ r) = evaluate r                     -- [Eval-UAndR]
evaluate !(BVOp l _ Unit) = evaluate l                     -- [Eval-UAndL]

evaluate !(BVOp l And x@(B _)) = do _ <- evaluate x; evaluate l
evaluate !(BVOp x@(B _) And r) = do _ <- evaluate x; evaluate r

evaluate !(BVOp l op x@(C _ _ _)) =
  -- trace ("recursive RChc with: \n") $
  do l' <- accumulate l
     let !res = BVOp l' op x
     if isValue l' && op == And
       then evaluate res
       else return res

evaluate !(BVOp x@(C _ _ _) op r) =
  -- trace ("recursive LChc \n") $
  do r' <- accumulate r
     let !res = BVOp x op r'
     if isValue r' && op == And
       then evaluate res
       else return res

  -- evaluation of two bools, the case that does the actual work
evaluate !(BVOp (B bl) op (B br)) =                     -- [Eval-Bools]
  -- trace "contracting" $
  evaluate (B res)
  where !res = (bDispatch op bl br)
        -- !name = ln ++ (pure $ toText op) ++ rn

evaluate !(BVOp l And r) =                                      -- [Eval-And]
  -- trace ("recursive AND case: \n") $
  do l' <- evaluate l
     r' <- evaluate r
     let !res = (BVOp l' And r')
     -- trace ("recursive AND case GOT RES:\n") $ return ()
     if isValue l' || isValue r'
       then evaluate res
       else return res

evaluate !(BVOp l op r) =                                         -- [Eval-Or]
  -- trace ("recursive GEn case: \n") $
  do l' <- accumulate l
     r' <- accumulate r
     let !res = (BVOp l' op r')
     -- trace ("recursive GEN case: \n") $ return ()
     if isValue l' && isValue r'
       then evaluate res
       else return res


accumulate :: Resultable d => BValue d -> IncVSMTSolve d (BValue d)
accumulate !Unit        = return Unit                             -- [Acc-Unit]
accumulate !(x@(B _))   = return x                                -- [Acc-Term]
accumulate !(x@(C _ _ _)) =
  -- trace "Ac: singleton chc" $
  return x                                -- [Acc-Chc]
accumulate !(x@(BVOp (C _ _ _) _ (C _ _ _))) =  return x          -- [Acc-Op-ChcL]
accumulate !(x@(BVOp (C _ _ _) _ (B _)))   =  return x          -- [Acc-Op-ChcL]
accumulate !(x@(BVOp (B _) _ (C _ _ _)))   =  return x          -- [Acc-Op-ChcR]

-- accumulate (BVOp Unit _ Unit) = trace "double unit" $ return Unit
accumulate !(BVOp Unit _ r) =
  -- trace ("AC LUNIT") $
  accumulate r                     -- [Acc-UAndR]
accumulate !(BVOp l _ Unit) =
  -- trace ("Ac RUnit") $
  accumulate l                     -- [Acc-UAndL]

accumulate !(BVOp l op x@(C _ _ _)) =
  -- trace "AC RCHC" $
  do l' <- accumulate l; return $! BVOp l' op x

accumulate !(BVOp x@(C _ _ _) op r) =
  -- trace "AC LCHC" $
  do r' <- accumulate r; return $! BVOp x op r'

  -- accumulation of two bools
accumulate !(BVOp (B bl) op (B br)) =                    -- [Acc-Bools]
  -- trace "Ac: Contracting bools" $
  return (B res)
  where !res = (bDispatch op bl br)
        -- !name = ln ++ (pure $ toText op) ++ rn

accumulate !(BVOp l op r) =                                       -- [Acc-Or]
  -- trace ("Accum recursive case") $
  do l' <- accumulate l;
     r' <- accumulate r;
     let !res = (BVOp l' op r')
     if isValue l' && isValue r'
       then accumulate res
       else return res

-- | Given a bvalue, return a symbolic reference of the negation of that bvalue
driveNotDown :: Resultable d => VProp d (S.SBool, Name) SNum -> BValue d
driveNotDown (LitB b)       = (B . bnot . S.literal $ b)
driveNotDown (RefB (b,_))   = (B $! bnot b)
driveNotDown (ChcB d l r)   = (C d (bnot l) (bnot r))                --[Acc-Neg-Chc]
driveNotDown (OpB Not e)    = toBValue e
driveNotDown (OpBB And l r) = BVOp l' Or r'
  where l' = driveNotDown l
        r' = driveNotDown r
driveNotDown (OpBB Or l r) = BVOp l' And r'
  where l' = driveNotDown l
        r' = driveNotDown r
driveNotDown (OpBB XOr l r)  = driveNotDown prop
  where prop = OpBB Or (OpBB And l (bnot r)) (OpBB And (bnot l) r)
driveNotDown (OpBB Impl l r) = driveNotDown (OpBB Or l' r)
  where l' = OpB Not l
driveNotDown (OpBB BiImpl l r) = driveNotDown prop
  where prop = (OpBB Or (OpBB And l r) (OpBB And (bnot l) (bnot r)))
driveNotDown (OpIB _ _ _) = error "Not implemented yet!"


isValue :: BValue d -> Bool
isValue (B _) = True
isValue Unit    = True
isValue _       = False

-- | After partial evaluation we need to use a zipper to find the most shallow
-- choice and begin evaluating choices. Ctx is the zipper
data Ctx d = InL (Ctx d) BB_B (BValue d)
           | InR !S.SBool BB_B (Ctx d)
           | Top
           deriving Show

type Loc d = (BValue d, Ctx d)

doChoice :: (Show d, Resultable d) => Loc d -> IncVSMTSolve d S.SBool
doChoice (b@(B b'), Top)                =
  -- trace ("Bool" ++ show b ++ "\n") $
  do _ <- evaluate b
     return b'
doChoice (Unit, Top)               =
  -- trace ("Unit Top \n") $
  return true
doChoice x@(C d l r, Top)            =
  -- trace ("Choice Top"  ++ show x ++ "\n") $
  do let
      bl = toBValue l
      goLeft = solveVariant $ doChoice (bl, Top)

      br = toBValue r
      goRight = solveVariant $ doChoice (br, Top)

     handleChc goLeft goRight d >>= store
     return true

doChoice (Unit, c@(InL parent op r))   =
  -- trace ("Unit L " ++ show c ++ "\n") $
  doChoice (r, parent)
doChoice (Unit, c@(InR acc op parent)) =
  -- trace ("Unit R " ++ show c ++ "\n") $
  doChoice (B acc, parent)

  -- when we see two bools we can evaluate them and climb the tree
doChoice (bl@(B _), InL parent op br@(B _)) =
  -- trace ("Accumulate case L \n") $
  accumulate (BVOp bl op br) >>= doChoice . mkCtx parent

doChoice (br@(B _), InR acc op parent) =
  -- trace ("Accumulate case R \n") $
  accumulate (BVOp (B acc) op br) >>= doChoice . mkCtx parent
  -- when we reach a bool leaf we set it as the accumulate and switch to the
  -- right fold
doChoice (B bl, InL parent op r) =
  -- trace ("Changing to fold case\n") $
  doChoice (r, InR bl op parent)

  -- when we find a choice we solve it recursively
doChoice x@(C d l r, ctx@(InL _ _ _)) =
  -- trace ("Got here L " ++ show x ++ "\n") $
  do let
      goLeft = do
        -- bl <- evaluate (toBValue l)
        solveVariant $! doChoice (toBValue l, ctx)

      goRight = do
        -- br <- evaluate (toBValue r)
        solveVariant $! doChoice (toBValue r, ctx)

     handleChc goLeft goRight d >>= store
     return true

doChoice x@(C d l r, ctx@(InR _ _ _)) =
  -- trace ("Got here R" ++ show x ++ "\n") $
  do let
      bl = toBValue l
      goLeft = solveVariant $ doChoice (bl, ctx)

      br = toBValue r
      goRight = solveVariant $ doChoice (br, ctx)

     handleChc goLeft goRight d >>= store
     return true

  -- we recur to the left in order to fold to the right
doChoice (BVOp l op r, ctx) =
  -- trace ("Recursive match\n") $
  doChoice (l, InL ctx op r)

mkTop :: BValue d -> Loc d
mkTop = mkCtx Top

mkCtx :: Ctx d -> BValue d -> Loc d
mkCtx = flip (,)

getFocus :: S.SBool -> Ctx d -> Loc d
getFocus b Top                     = (B b, Top)
getFocus b (InL parent op rbranch) = (rbranch, InR b op parent)
getFocus b (InR acc op Top)        = (B $! bDispatch op acc b, Top)
getFocus b (InR acc op (InL parent op' rbranch)) = (rbranch, InR res op' parent)
  where !res = bDispatch op acc b
getFocus b (InR acc op (InR acc' op' parent)) = (B res', parent)
  where !res = bDispatch op acc b
        !res' = bDispatch op' acc' res



-- data Ctx d = InL (Ctx d) BB_B (BValue d)
--            | InR !S.SBool BB_B (Ctx d)
--            | Top

-- | Given a Loc find the first choice by crawling down the left hand of the zipper
findPrincipleChoice :: Loc d -> Loc d
  -- base casee
findPrincipleChoice x@(C _ _ _, _)   = x
  -- terminal cases we want to avoid
findPrincipleChoice (BVOp (B x) op r, ctx) = findPrincipleChoice (r, InR x op ctx)
findPrincipleChoice (BVOp Unit op r, ctx) = findPrincipleChoice (r, InR true op ctx)
  -- recursive cases
findPrincipleChoice (BVOp l op r, ctx) = findPrincipleChoice (l, InL ctx op r)
  -- totalize over the context if all these fail and let doChoice handle it
findPrincipleChoice x = x

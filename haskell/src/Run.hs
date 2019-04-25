module Run ( SatDict
           , Log
           , runAD
           , runBF
           , runVSMT
           , fst'
           , IncPack
           , smtBoolWith
           , smtInt
           ) where

import           Control.Arrow (first, second)
import           Control.DeepSeq (force)
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict as St
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

import Debug.Trace
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
  where x' = foldr' ($) x (opts conf)

runAD :: (Show d, Resultable d) =>
         SMTConf d Text Text
      -> ReadableProp d
      -> (d -> Text)
      -> IO (Result d)
runAD os p f = fst' <$> runEnv (flip runAndDecomp f) os p

runBF :: (Show d, Resultable d) =>
         SMTConf d Text Text
      -> ReadableProp d
      -> IO (Result d)
runBF os p = fst' <$> runEnv runBruteForce os p

-- | Run the VSMT solver given a list of optimizations and a prop
runVSMT :: (Show d, Resultable d) =>
           ConfigPool d        ->
           SMTConf d Text Text ->

           ReadableProp d      ->
           IO (Result d, SatDict d, Log)
runVSMT = runEnv . runVSMTSolve

-- | Given a VProp a term generate the satisfiability map
initSt :: Ord d => VProp d a a -> SatDict d
initSt prop = sats
  where sats = M.fromList . fmap (\x -> (x, False)) $ choices prop

-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter [Char] m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter [Char] m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x

-- | run the sat solver, when we get a satisafiable call get the assignments
-- directly
runForDict :: Resultable d => S.Predicate -> IO (Result d)
runForDict x = S.runSMT $
  do x' <- x
     SC.query $
       do S.constrain x'
          getResultWith (toResultProp . LitB)

-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runBruteForce ::
  (MonadTrans t, Show d, Resultable d , MonadState (SatDict d) (t IO)) =>
  ReadableProp d -> t IO (Result d)
runBruteForce prop = lift $ flip evalStateT (initSt prop) $
  do
  _confs <- get
  let confs = M.keys _confs
      plainProps = if null confs
        then [Just (M.empty, prop)]
        else (\y -> sequence $ (y, selectVariant y prop)) <$> confs
  plainMs <- lift $
             mapM (bitraverse
                   (pure . configToResultProp)
                   (runForDict . symbolicPropExpr show show show)) $ catMaybes plainProps
  return $ foldMap (uncurry helper) plainMs
  where
        helper c as =  insertToSat c as

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (Show d, Resultable d, MonadTrans t, Monad (t IO)) =>
  ReadableProp d -> (d -> Text) -> t IO (Result d)
runAndDecomp prop f =
  lift $ runForDict $ symbolicPropExpr show show show $ andDecomp prop (f . dimName)

runVSMTSolve ::
  (Show d, MonadTrans t, Resultable d , MonadReader (SMTConf d a a) (t IO)) =>
  ConfigPool d ->
  ReadableProp d ->
  t IO (Result d)
runVSMTSolve configPool prop =
  do cnf <- ask
     -- convert all refs to SBools
     let prop' = St.evalStateT (propToSBool prop) (mempty, mempty)
     -- run the inner driver
     res <- lift . S.runSMTWith (conf cnf) $! vSMTSolve prop' configPool
     lift . return $ res

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

emptySt :: (Resultable d, Monoid d) => IncState d
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
     SC.query $
       do
         -- TODO expose this limit to user and find good default we take a chunk
         -- from the pool to process with because it may be infinite we pull an
         -- initial config to share its plain information on the assertion stack
         let (fstConfig, pool) = splitAt 1 $
                                 if length configPool > 50
                                 then take 50 configPool
                                 else configPool
         -- prep assertion stack with first configs plain terms
         (_, fstSt) <-
           if not $ null pool
           then do
             St.runStateT (solveVariational fstConfig prop') emptySt
           else return ((), emptySt)

         -- solve the rest of the pool and check if models where generated. If
         -- they weren't then the prop was plain (no choices) because we only
         -- grab models from choices
         (_,resSt) <- St.runStateT (solveVariational pool prop') fstSt
         if isResultNull (result resSt)
           then St.evalStateT (vSMTSolve_ prop') resSt >>= solvePlain . fst
           else return $ result resSt

solvePlain :: Resultable d => S.SBool -> SC.Query (Result d)
solvePlain b = do S.constrain b
                  b' <- isSat
                  if b'
                    then getResultWith (toResultProp . LitB)
                    else return mempty

solveVariational :: (Show d, Resultable d) =>
                    ConfigPool d ->
                    VProp d (S.SBool, Name) SNum ->
                    IncVSMTSolve d ()
solveVariational []        p    = do _ <- vSMTSolve_ p; return ()
solveVariational [x]       p    = do setConfig x; _ <- vSMTSolve_ p; return ()
solveVariational cs prop = mapM_ step cs
  where step cfg = do lift $ SC.push 1
                      setConfig cfg
                      _ <- vSMTSolve_ prop
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
setModelGenD = St.modify' $ onProcessed (const True)

setModelNotGenD :: IncVSMTSolve d ()
setModelNotGenD = St.modify' $ onProcessed (const False)

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

solveVariant :: (Resultable d, Show d) =>
  IncVSMTSolve d (I.SBool, ConstraintName) -> IncVSMTSolve d (Result d)
solveVariant go = do
           setModelNotGenD

           -- the recursive computaton
           lift $! SC.push 1
           go
           -- (b', n') <- case r of
           --               x@(B b n) -> return (b, n)
           --               x -> solveBValue x >>= doChoice
           -- constrain b' n'

           -- check if the config was satisfiable, and if the recursion
           -- generated a model
           bd <- hasGenDModel

           -- if not generated a model, then construct a -- result
           resMap <- if not bd
                     then do prop <- gets (configToResultProp . config)
                             setModelGenD
                             lift $ getResult prop
                      else -- not sat or have gen'd a model so ignore
                       return mempty

           -- reset stack
           lift $! SC.pop 1

           return resMap

handleChc :: (Show d, Resultable d) =>
  IncVSMTSolve d (Result d)
  -> IncVSMTSolve d (Result d)
  -> Dim d
  -> IncVSMTSolve d ()
handleChc goLeft goRight d =
  do currentCfg <- gets config
     case M.lookup d currentCfg of
       Just True  -> goLeft >>= store
       Just False -> goRight >>= store
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
         store $! resMapT <> resMapF

         -- we remove the Dim from the config so that when the recursion
         -- completes the last false branch of the last choice is not
         -- remembered. For Example if we have A<p,q> /\ B<r,s> then the
         -- recursion starts at A and completes at the right variants of
         -- B, namely s. If we don't remove the dimension from the config
         -- then the config will retain the selection of (B, False) and
         -- therefore on the (A, False) branch of the recursion it'll
         -- miss (B, True)
         removeDim d

data BValue d = B! S.SBool ConstraintName
              | Unit
              | C! (Dim d)
                (VProp d (S.SBool, Name) SNum)
                (VProp d (S.SBool, Name) SNum)
              | BVOp (BValue d) BB_B (BValue d)
              deriving Show


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
vSMTSolve_ :: (Show d, Resultable d) =>
  VProp d (S.SBool, Name) SNum -> IncVSMTSolve d (S.SBool , ConstraintName)
-- vSMTSolve_ p = vSMTSolve__ p >>= \x -> return $ trace (show x) $ solveBValue
-- x
vSMTSolve_ p = do res <- vSMTSolve__ p
                  trace ("BVALUE: " ++ ushow res ++ "\n") $ return ()
                  r' <- solveBValue res
                  trace ("Compacted: " ++ ushow r' ++ "\n") $ return ()
                  doChoice r'

vSMTSolve__ :: (Show d, Resultable d) =>
  VProp d (S.SBool, Name) SNum -> IncVSMTSolve d (BValue d)
vSMTSolve__ (RefB (b,name)) = return $! B b (pure name)
vSMTSolve__ (LitB b) = return $! B (S.literal b) (pure $ toText b)
vSMTSolve__ (OpB Not (OpB Not notchc)) = vSMTSolve__ notchc
vSMTSolve__ (OpB Not e) = do
  (B b n) <- vSMTSolve__ e
  return $! B (bnot b)  (toText Not : n)

vSMTSolve__ x@(OpBB op (RefB (b,n)) (RefB (b',n'))) =
  trace ("[VDBG]: Reducing: " ++ ushow x ++ "\n") $
  do
  let bres = (bDispatch op) b b'
      name = [n,toText op,n']
  return (B bres name)

vSMTSolve__ x@(OpBB op (ChcB d l r) r') =
  trace ("[VDBG]: Chc in LHS: " ++ ushow x ++ "\n") $
  do br <- vSMTSolve__ r'
     return $! BVOp (C d l r) op br

vSMTSolve__ x@(OpBB op l' (ChcB d l r)) =
  trace ("[VDBG]: Chc in RHS: " ++ ushow x ++ "\n") $
  do bl <- vSMTSolve__ l'
     return $! BVOp bl op (C d l r)

-- vSMTSolve__ x@(OpBB Impl l r) =
--   trace ("[VDBG]: Implication Removed: " ++ ushow x ++ "\n") $ do
--   bvl <- vSMTSolve__ (bnot l)
--   bvr <- vSMTSolve__ r
--   let bres = BVOp bvl Or bvr
--   return $! bres

-- vSMTSolve__ x@(OpBB BiImpl l r) =
--   trace ("[VDBG]: BiImpl removed: " ++ ushow x ++ "\n") $ do
--   bvl <- vSMTSolve__ (l ==> r)
--   bvr <- vSMTSolve__ (r ==> l)
--   let bres = BVOp bvl And bvr
--   return $! bres

vSMTSolve__ x@(OpBB op l r) =
  trace ("[VDBG]: Reccurring: " ++ ushow x ++ "\n") $
  do
  bvl <- vSMTSolve__ l
  bvr <- vSMTSolve__ r
  let bres = BVOp bvl op bvr
  return $! bres

vSMTSolve__ (OpIB _ _ _) = error "Blame Jeff! This isn't implemented yet!"
vSMTSolve__ x@(ChcB d l r) =
  trace ("[VDBG]: Singleton CHC: " ++ ushow x ++ "\n") $
  return $! (C d l r)


solveBValue :: (Show d, Resultable d) => BValue d -> IncVSMTSolve d (BValue d)
solveBValue Unit = return Unit
solveBValue x@(B b n) =
  trace ("[DBG]: constraining: " ++ ushow x ++ "\n") $
  do constrain b n; return Unit
solveBValue x@(C _ _ _) = return x

solveBValue x@(BVOp (C _ _ _) _ (B _ _)) = return x
solveBValue x@(BVOp (B _ _) _ (C _ _ _)) = return x

solveBValue x@(BVOp Unit _ l) =
  trace ("[DBG]: Removing Unit: " ++ ushow x ++ "\n") $
  solveBValue l
solveBValue x@(BVOp l _ Unit) =
  trace ("[DBG]: Removing Unit: " ++ ushow x ++ "\n") $
  solveBValue l

solveBValue y@(BVOp l op x@(C _ _ _)) =
  trace ("[DBG]: Chc in Right, Recurring Left: " ++ ushow y ++ "\n") $
  do
  l' <- solveBValue l
  trace ("[DBG]: went left got: " ++ ushow l' ++ "\n") $ return ()
  return $! BVOp l' op x

solveBValue y@(BVOp x@(C _ _ _) op r) =
  trace ("[DBG]: Chc in Left: " ++ ushow y ++ "\n") $
  do
  r' <- solveBValue r
  trace ("[DBG]: went right got: " ++ ushow r' ++ "\n") $ return ()
  doBVOp x op r'

-- solveBValue x@(BVOp l Or r) =
--   trace ("[DBG]: LHS Splitting: " ++ ushow l ++ "\n") $
--   trace ("[DBG]: RHS Splitting: " ++ ushow r ++ "\n") $
--      handleOr l r


-- solveBValue x@(BVOp l And r) =
--   trace ("[DBG]: LHS Splitting: " ++ ushow l ++ "\n") $
--   trace ("[DBG]: RHS Splitting: " ++ ushow r ++ "\n") $
--   do _ <- solveBValue l; solveBValue r

solveBValue x@(BVOp (B bl ln) op (B br rn)) =
  trace ("[DBG]: Reducing: " ++ ushow x ++ "\n") $
  solveBValue $! B res name
  where res = (bDispatch op bl br)
        name = ln ++ (pure $ toText op) ++ rn

solveBValue x@(BVOp l op r) =
  trace ("[DBG]: Recurring: " ++ ushow x ++ "\n") $

  do bl <- solveBValue l
     br <- solveBValue r
     doBVOp bl op br

doBVOp :: (Show d, Resultable d) =>
  BValue d -> BB_B -> BValue d -> IncVSMTSolve d (BValue d)
doBVOp l    _ Unit = solveBValue l
doBVOp Unit _ r    = solveBValue r
doBVOp l@(B _ _)   op r@(B _ _) = solveBValue $ BVOp l op r
doBVOp l@(B _ _)   op c@(C _ _ _)   = return $ BVOp l op c
doBVOp c@(C _ _ _) op l@(B _ _)     = return $ BVOp c op l
doBVOp c@(C _ _ _) op c'@(C _ _ _)  = return $ BVOp c op c'
-- doBVOp c@(BVOp _ _ _) op c'@(C _ _ _) =
--   do res <- solveBValue c; doBVOp res op c'
-- doBVOp c@(C _ _ _) op c'@(BVOp _ _ _) =
--   do res <- solveBValue c'; doBVOp c op res
-- doBVOp c@(BVOp _ _ _) op c'@(B _ _) =
--   do res <- solveBValue c; doBVOp res op c'
-- doBVOp c@(B _ _) op c'@(BVOp _ _ _) =
--   do res <- solveBValue c'; doBVOp c op res
doBVOp l op r = return $ BVOp l op r


doChoice :: (Show d, Resultable d) => BValue d -> IncVSMTSolve d (S.SBool, ConstraintName)
doChoice Unit = return (true, mempty)
doChoice (B b n) = return (b , n)
doChoice (C d l r) =
  do bl <- vSMTSolve__ l
     br <- vSMTSolve__ r
     let goLeft = solveVariant $ solveBValue bl >>= doChoice
         goRight = solveVariant $ solveBValue br >>= doChoice
     handleChc goLeft goRight d

     return (true, mempty)

doChoice x@(BVOp (C d l r) op r') =
  do bl <- vSMTSolve__ l
     br <- vSMTSolve__ r
     let goLeft = solveVariant ((solveBValue $ BVOp bl op r') >>= doChoice)
         goRight = solveVariant ((solveBValue $ BVOp br op r') >>= doChoice)
     handleChc goLeft goRight d

     return (true, mempty)

doChoice x@(BVOp l' op (C d l r)) =
  trace ("[DBG]: Eval'ing Choice in RHS: " ++ ushow x ++ "\n") $
  do bl <- vSMTSolve__ l
     br <- vSMTSolve__ r
     let goLeft = solveVariant ((solveBValue $ BVOp l' op bl) >>= doChoice)
         goRight = solveVariant ((solveBValue $ BVOp l' op br) >>= doChoice)
     handleChc goLeft goRight d

     return (true, mempty)

doChoice (BVOp l op r) =
  do (lb, ln) <- doChoice l
     (rb, rn) <- doChoice r
     return ((bDispatch op) lb rb, ln ++ [toText op] ++ rn)

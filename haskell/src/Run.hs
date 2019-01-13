module Run ( Result (..)
           , SatDict
           , Log
           , runAD
           , runBF
           , runVSMT
           , fst'
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict    as St
import qualified Data.SBV.Internals  as I
import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC
import           Prelude hiding (LT, GT, EQ)
import Data.Foldable (foldr')
import Data.Text (Text)
import qualified Data.Sequence as SE

import Debug.Trace (trace)

import Control.Arrow                 (first, second)

import Data.Maybe                    (fromJust, catMaybes)

import VProp.Types
import VProp.SBV
import VProp.Core
import V
import Utils
import Config

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- (an mapping from dimensions to booleans denoting selection) and values are
-- whether that config is satisfiable or not (a bool)
type SatDict a = M.Map (Config a) Bool

-- | Type convenience for Log
type Log = Text

-- | Takes a dimension d, a type for values a, and a result r
type Env d a r = RWST (SMTConf d a a) Log (SatDict d) IO r -- ^ the monad stack

-- | A zipper for evaluation. This is required so that everytime we have a
-- choice we have access to the rest of the proposition that we are evaluation.
-- If we do not have access then there is no way to perform a selection,
-- manipulate the assertion stack and then continue evaluating.
-- data Op = CAnd | COr deriving Show

-- type OpNCtx d a b = Opn (SE.Seq (VProp d a b), S.SBool)

-- data BBCtx d a b =
--   -- ^ in a left branch of a tree we have a focus, a context that
--   -- represents the parent of the focus and a right branch of a
--   -- tree

--   -- ^ in a right branch of a tree we have a focus, a left branch
--   -- and a context for the parent
--   deriving Show

-- | A Ctx is a zipper over the VProp data type. This is used in the vsmtsolve
-- routine to maintain a context when a choice is observed. This way we always
-- have a reference to the rest of the formula we are solving during a
-- variational solve. Its a sum type over the zipper for n-ary operators like
-- And and Or, or its a zipper over binary operators like ==> <=>. For binary
-- operators we keep a prop that is the focus and a context which represents the
-- parent and adjacent child
data Ctx d a b = N Opn S.SBool (SE.Seq (VProp d a b)) (Ctx d a b)
               | InBBL BB_B (Ctx d a b) (VProp d a b)
               | InBBR BB_B (VProp d a b) (Ctx d a b)
               | Empty
               deriving Show


type Loc d a b = (VProp d a b, Ctx d a b)

-- | An empty reader monad environment, in the future read these from config file
_emptySt :: SatDict d
_emptySt = M.empty

-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Show a =>
  Env d a r -> SMTConf d a a -> SatDict d -> IO (r, (SatDict d),  Log)
_runEnv m opts st = runRWST m opts st

-- TODO use configurate and load the config from a file
runEnv :: Show a => (VProp d a a -> Env d a (Result d))
       -> SMTConf d a a
       -> VProp d a a -> IO (Result d, SatDict d, Log)
runEnv f conf !x = _runEnv (f x') conf _emptySt
  where !x' = foldr' ($!) x (opts conf)

runAD :: (Show a, Show d, Ord d, Ord a) =>
         SMTConf d a a
      -> VProp d a a
      -> (d -> a)
      -> IO (Result d)
runAD os p f = fst' <$> runEnv (flip runAndDecomp f) os p

runBF :: (Show a, Show d, Ord a, Ord d) =>
         SMTConf d a a
      -> VProp d a a
      -> IO (Result d)
runBF os p = fst' <$> runEnv runBruteForce os p

-- | Run the VSMT solver given a list of optimizations and a prop
runVSMT :: (Show d,
            Show a, Ord a, Ord d) =>
           SMTConf d a a
        -> VProp d a a
        -> IO (Result d, SatDict d, Log)
runVSMT = runEnv runVSMTSolve

-- | Given a VProp a term generate the satisfiability map
initSt :: Ord d => VProp d a a -> SatDict d
initSt prop = sats
  where sats = M.fromList . fmap (\x -> (x, False)) $ M.fromList <$> choices prop

-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter [Char] m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter [Char] m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x


-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runBruteForce ::
  (MonadTrans t, Show a,Show d, Ord a, Ord d, MonadState (SatDict d) (t IO)) =>
  VProp d a a -> t IO (Result d)
runBruteForce prop = lift $ flip evalStateT (initSt prop) $
  do
  _confs <- get
  let confs = M.keys _confs
      plainProps = (\y -> sequence $ (y, selectVariant y prop)) <$> confs
  plainMs <- lift $ mapM (bitraverse pure (fmap unsat . S.sat . symbolicPropExpr)) $ catMaybes plainProps
  return . Result . bimap dimName Just .  fromJust $ recompile plainMs
  where unsat (S.SatResult smtModel) = smtModel

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (Show a, Show d, Ord d, Ord a, MonadTrans t, Monad (t IO)) =>
  VProp d a a -> (d -> a) -> t IO (Result d)
runAndDecomp prop f = do
  res <- lift . S.runSMT $ do
    p <- symbolicPropExpr $ andDecomp prop (f . dimName)
    SC.query $ do S.constrain p; getVSMTModel
  lift . return . Result $ Plain res

runVSMTSolve ::
  (Show d,
   Show a, Ord a, Ord d, MonadTrans t, MonadReader (SMTConf d a a) (t IO)) =>
  VProp d a a -> t IO (Result d)
runVSMTSolve prop =
  do cnf <- ask
     res <- lift . S.runSMTWith (conf cnf) . vSMTSolve $
       St.evalStateT (propToSBool prop) (M.empty, M.empty)
     lift . return . Result $ res

-- | main workhorse for running the SAT solver
newtype Result a = Result {unRes :: V a (Maybe S.SMTResult)}

-- | wrapper around map to keep track of the variable references we've seen, a,
-- and their symbolic type, b
type UsedVars a b = M.Map a b

-- | A state monad transformer that holds two usedvar maps, one for booleans and
-- one for doubles
type IncPack a b = St.StateT ((UsedVars a S.SBool, UsedVars a SNum)) S.Symbolic b

-- | a map to keep track if a dimension has been seen before
type UsedDims a = M.Map a Bool

-- | the internal state for the incremental solve algorithm, it holds a result
-- list, and the used dims map, and is parameterized by the types of dimensions,
-- d
type IncState d a = (V d (Maybe a), UsedDims (Dim d))

-- | the incremental solve monad, with the base monad being the query monad so
-- we can pull out sbv models Hardcoding so that I don't have to write the mtl
-- typeclass. I do not expect these to change much
type IncVSMTSolve d a = St.StateT (IncState d S.SMTResult) SC.Query a

-- | we need to define an Eq on SMTResult for recompiling. If we don't have this
-- then during recompile in the BF routine we cannot erase dead choices
instance Eq S.SMTResult where
  x == y
    | S.modelExists x && S.modelExists y = S.getModelDictionary x == S.getModelDictionary y
    | otherwise = matchResults x y
    where matchResults (S.Unsatisfiable _) (S.Unsatisfiable _) = True
          matchResults (S.Satisfiable _ _) (S.Satisfiable _ _) = True
          matchResults (S.SatExtField _ _) (S.SatExtField _ _) = True
          matchResults (S.Unknown _ _)     (S.Unknown _ _)     = True
          matchResults (S.ProofError _ _)  (S.ProofError _ _)  = True
          matchResults _                   _                   = False

instance Eq S.SatResult where (S.SatResult x) == (S.SatResult y) = x == y
instance Eq S.ThmResult where (S.ThmResult x) == (S.ThmResult y) = x == y

-- | Top level wrapper around engine and monad stack, this sets options for the
-- underlying solver, inspects the results to see if they were variational or
-- not, if not then it gets the model and wraps it in a V datatype
vSMTSolve :: (Ord d, Show d) => S.Symbolic (VProp d S.SBool SNum)
          -> S.Symbolic (V d (Maybe S.SMTResult))
vSMTSolve prop = do prop' <- prop
                    S.setOption $ SC.ProduceAssertions True
                    SC.query $
                      do
                      (b, (res',_)) <- St.runStateT (vSMTSolve_ prop')
                              (Plain Nothing, M.empty)
                      res <- if V.isPlain res'
                             then do S.constrain b
                                     prf <- SC.getSMTResult
                                     return . Plain . Just $ prf
                             else return res'
                      return res

-- | This ensures two things: 1st we need all variables to be symbolic before
-- starting query mode. 2nd we cannot allow any duplicates to be called on a
-- string -> symbolic a function or missiles will launch.
propToSBool :: (Show a,Ord a) => VProp d a a -> IncPack a (VProp d S.SBool SNum)
propToSBool !(RefB x)     = RefB   <$> smtBool x
propToSBool !(OpB o e)    = OpB  o <$> propToSBool e
propToSBool !(OpBB o l r) = OpBB o <$> propToSBool l <*> propToSBool r
propToSBool !(Opn o xs)   = Opn  o <$> traverse propToSBool xs
propToSBool !(ChcB d l r) = ChcB d <$> propToSBool l <*> propToSBool r
propToSBool !(OpIB o l r) = OpIB o <$> propToSBool' l <*> propToSBool' r
propToSBool !(LitB b)     = return $ LitB b

propToSBool' :: (Ord b, Show b) => VIExpr d b -> IncPack b (VIExpr d SNum)
propToSBool' !(Ref RefI i) = Ref RefI <$> smtInt i
propToSBool' !(Ref RefD d) = Ref RefD <$> smtDouble d
propToSBool' !(OpI o e)    = OpI o    <$> propToSBool' e
propToSBool' !(OpII o l r) = OpII o <$> propToSBool' l <*> propToSBool' r
propToSBool' !(ChcI d l r) = ChcI d <$> propToSBool' l <*> propToSBool' r
propToSBool' !(LitI x)     = return $ LitI x

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
                                   St.modify (second $ M.insert str b')
                                   return b'
                     Just x  -> return x


-- | convert every reference to a boolean, keeping track of what you've seen
-- before, can't use mkStatement here because its a special case
smtBool :: (Show a, Ord a) => a -> IncPack a S.SBool
smtBool str = do (st,_) <- get
                 case str `M.lookup` st of
                   Nothing -> do b <- lift . S.sBool $ show str
                                 St.modify (first $ M.insert str b)
                                 return b
                   Just x  -> return x

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtInt :: (Show a, Ord a) => a -> IncPack a SNum
smtInt = mkSmt S.sInt64 SI

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtDouble :: (Show a, Ord a) =>  a -> IncPack a SNum
smtDouble = mkSmt S.sDouble SD

getVSMTModel :: SC.Query (Maybe S.SMTResult)
getVSMTModel = do cs <- SC.checkSat
                  case cs of
                    SC.Unk   -> error "Unknown Error from solver!"
  -- if unsat the return unsat, just passing default config to get the unsat
  -- constructor TODO return correct conf
                    SC.Unsat -> return . Just $ S.Unsatisfiable S.defaultSMTCfg
                    SC.Sat   -> SC.getSMTResult >>= return . pure

-- | type class needed to avoid lifting for constraints in the IncSolve monad
instance (Monad m, I.SolverContext m) =>
  I.SolverContext (StateT (IncState d a) m) where
  constrain = lift . S.constrain
  namedConstraint = (lift .) . S.namedConstraint
  setOption = lift . S.setOption

-- | smartly grab a result from the state checking to make sure that if the
-- result is variational than that is preferred over a redundant model. Or in
-- other words, models only occur in leaves of the V tree
getResult :: IncVSMTSolve d (V d (Maybe S.SMTResult))
getResult = do (res, _) <- get
               if isEmpty res
                 then lift getVSMTModel >>= return . Plain
                 else return res
  where isEmpty (Plain Nothing) = True
        isEmpty _               = False

clearSt :: IncVSMTSolve d ()
clearSt = St.modify . first $ const (Plain Nothing)

store :: V d (Maybe S.SMTResult) -> IncVSMTSolve d ()
store = St.modify . first . const

handleChc goLeft goRight defL defR (ChcB d _ _) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> defL
       Just False -> defR
       Nothing    -> do
                        clearSt
                        St.modify . second $ M.insert d True
                        lift $! SC.push 1
                        goLeft >>= S.constrain
                        lRes <- getResult
                        lift $! SC.pop 1

                        clearSt
                        St.modify . second $ M.adjust (const False) d
                        lift $! SC.push 1
                        goRight >>= S.constrain
                        rRes <- getResult
                        lift $! SC.pop 1

                        store $ VChc (dimName d) lRes rRes
                        St.modify . second $ M.delete d
     -- this return statement should never matter because we've reset the
     -- assertion stack. So I just return true here to fulfill the type
                        return true

-- | Abstracts out the choice handling for use in handling contexts.
handleChcCtx :: (Show d, Ord d) =>
  Loc d S.SBool SNum ->
  Loc d S.SBool SNum ->
  VProp d b c -> IncVSMTSolve d S.SBool
handleChcCtx goLeft goRight c = handleChc
  (handleCtx goLeft) (handleCtx goRight)
  (handleCtx goLeft) (handleCtx goRight) c

-- | abstracts out the choice handling for use in the vSMTsolve function.
-- Purposefully a partial function
handleChcVSMT :: (Show d, Ord d) =>
  VProp d S.SBool SNum ->
  VProp d S.SBool SNum ->
  VProp d S.SBool SNum -> IncVSMTSolve d S.SBool
handleChcVSMT goLeft goRight c@(ChcB _ l r) =
  handleChc
  (vSMTSolve_ goLeft) (vSMTSolve_ goRight)
  (vSMTSolve_ l) (vSMTSolve_ r) c

-- | Handle a choice in the IncVSMTSolve monad, we check to make sure that if a
-- choice is already selected then the selection is maintained. If not then we
-- solve both branches by first clearing the state, recursively solving, if the
-- recursive answer is plain then we'll get back a model from the solver, if not
-- then we'll get back a variational model, if we get back a variational model
-- then we reconstruct the choice expression representing the model and store it
-- in the state. This works by having a zipper (focus, context) that tracks the
-- recurses through the proposition. When the prop is plain it will simple
-- convert everything to SBools via vSMTSolve and store that is the ctx which is
-- the constrained in runVSMTsolve. If there is a choice then we employ the
-- context to solve recursively by selecting a variant as our new focus and
-- recurring.
handleCtx :: (Ord d, Show d) => Loc d S.SBool SNum -> IncVSMTSolve d S.SBool
handleCtx (p, Empty) = vSMTSolve_ p
handleCtx (c@(ChcB _ l r), N op acc rest ctx) =
  handleChcCtx goLeft goRight c
  -- when we see a choice as our focus we check if it is selected. If so then
  -- recur. If not then we select a variant and pass that on with the current
  -- context.
  where goLeft  = (l, N op acc rest ctx)
        goRight = (r, N op acc rest ctx)

handleCtx (fcs, N op acc SE.Empty Empty) =
  vSMTSolve_ fcs >>= return . (handler op) acc
  where handler And = (S.&&&)
        handler Or  = (S.|||)

handleCtx (fcs, N op acc SE.Empty ctx) =
  handleCtx (fcs, ctx)
  where handler And = (S.&&&)
        handler Or  = (S.|||)
  -- when we have reached the end of the sequence we simple return the
  -- accumulated SBool. Yes this is some funky fold
handleCtx (fcs, N op acc (nfcs SE.:<| rest) ctx) =
  -- when we do not see a choice we peel off the focus and convert it to a SBool
  -- with vSMTsolve and then accumulate it appropriately with the operator
  -- this is the engine of the algorithm, its the point that makes progress
  do
  fcs' <- vSMTSolve_ fcs
  let newAcc = (handler op) fcs' acc
  handleCtx (nfcs, N op newAcc rest ctx)
  -- handleCtx (nfcs, (N op newAcc rest ctx)) >>= return . ((handler op) fcs')
  where handler And = (S.&&&)
        handler Or  = (S.|||)

handleCtx (c@(ChcB d l r), (InBBL op ctx rbranch))=
  handleChcCtx goLeft goRight c

  where goLeft  = (l, InBBL op ctx rbranch)
        goRight = (r, InBBL op ctx rbranch)
handleCtx (Opn And (p SE.:<| ps), InBBL op Empty rbranch) =
  do
    ls <- handleCtx (p, N And true ps Empty)
    rbranch' <- vSMTSolve_ rbranch
    return $ (handler op) ls rbranch'
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)

handleCtx (Opn Or (p SE.:<| ps), InBBL op Empty rbranch) =
  do
    handleCtx (p, N Or false ps (InBBL op Empty rbranch))
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
handleCtx (fcs, InBBL op Empty rchild) =
  do
    fcs' <- vSMTSolve_ fcs
    rchild' <- vSMTSolve_ rchild
    return $ (handler op) rchild' fcs'
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
-- handleCtx (B InBBL op c@(ChcB d l r) ctx rbranch) = handleChcCtx goLeft goRight c
--   where handler Impl   = (==>)
--         handler BiImpl = (<=>)
--         handler XOr    = (<+>)

--         goLeft = InBBL op l ctx rbranch
--         goRight = InBBL op r ctx rbranch
-- handleCtx (InBBL op Empty rbranch) = vSMTSolve_ $ OpBB op rbranch
-- handleCtx (InBBL op ctx rbranch) = vSMTSolve_ $ OpBB op fcs rbranch

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl
vSMTSolve_ :: (Ord d, Show d) => VProp d S.SBool SNum -> IncVSMTSolve d S.SBool
vSMTSolve_ !(RefB b) = return b
vSMTSolve_ !(LitB b) = return $ S.literal b
vSMTSolve_ !(OpB Not c@(ChcB _ l r)) = handleChcVSMT goLeft goRight c
  where goLeft = OpB Not l
        goRight = OpB Not r
vSMTSolve_ (OpB Not (OpB Not notchc)) = vSMTSolve_ notchc
vSMTSolve_ (OpB Not notchc) = S.bnot <$> vSMTSolve_ notchc
-- vSMTSolve_ !(OpBB op c@(ChcB _ cl cr) r) = handleChcVSMT goLeft goRight c
--   where goLeft = OpBB op cl r
--         goRight = OpBB op cr r
-- vSMTSolve_ !(OpBB op l c@(ChcB _ cl cr)) = handleChcVSMT goLeft goRight c
--   where goLeft = OpBB op l cl
--         goRight = OpBB op l cr
vSMTSolve_ (OpBB op l r) = handleCtx $ (l, InBBL op Empty r)
-- vSMTSolve_ a@(OpBB op l r) = do bl <- vSMTSolve_ l
--                                 br <- vSMTSolve_ r
--                                 let op' = handler op
--                                 return $ bl `op'` br
--   where handler Impl   = (==>)
--         handler BiImpl = (<=>)
--         handler XOr    = (<+>)
vSMTSolve_ !(OpIB op l r) = do l' <- vSMTSolve'_ l
                               r' <- vSMTSolve'_ r
                               _ <- reifyArithChcs l' r' (handler op)
  -- this result should, and will never matter the return values from the
  -- handler are handled in side effects
                               return true
  where handler LT  = (.<)
        handler LTE = (.<=)
        handler GTE = (.>=)
        handler GT  = (.>)
        handler EQ  = (.==)
        handler NEQ = (./=)

vSMTSolve_ !(Opn And (p SE.:<| ps)) = handleCtx $ (p, N And true ps Empty)
vSMTSolve_ !(Opn Or (p SE.:<| ps))  = handleCtx $ (p, N Or false  ps Empty)
vSMTSolve_ x = handleCtx (x, N And true SE.Empty Empty)


handleSBoolChc :: Ord d => V (Dim d) S.SBool -> IncVSMTSolve d S.SBool
handleSBoolChc !(Plain a) = do S.constrain a
                               return a
handleSBoolChc !(VChc d l r) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> handleSBoolChc l
       Just False -> handleSBoolChc r
       Nothing    -> do clearSt
                        St.modify . second $ M.adjust (const True) d
                        lift $ SC.push 1
                        l' <- handleSBoolChc l
                        S.constrain l'
                        lRes <- getResult
                        lift $ SC.pop 1

                        clearSt
                        St.modify . second $ M.insert d False
                        lift $ SC.push 1
                        r' <- handleSBoolChc r
                        S.constrain r'
                        rRes <- getResult
                        lift $ SC.pop 1

                        store $ VChc (dimName d) lRes rRes
                        St.modify . second $ M.delete d

                        return r'

reifyArithChcs :: Ord d => V (Dim d) SNum -> V (Dim d) SNum
  -> (SNum -> SNum -> S.SBool)
  -> IncVSMTSolve d (V (Dim d) S.SBool)
reifyArithChcs !(Plain a) !(Plain b) op = return . Plain $ a `op` b
reifyArithChcs !(Plain a) !(VChc d l r) op =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> return $ l >>= return . op a
       Just False -> return $ r >>= return . op a
       Nothing    -> do let x = op a <$> (VChc d l r)
                        _ <- handleSBoolChc x
                        return x

reifyArithChcs !(VChc d l r) !(Plain a) op =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> return $ l >>= return . flip op a
       Just False -> return $ r >>= return . flip op a
       Nothing    -> do let x = flip op a <$> (VChc d l r)
                        _ <- handleSBoolChc x
                        return x

reifyArithChcs !(VChc ad al ar) !(VChc bd bl br) op =
  do (_, used) <- get
     case (M.lookup ad used, M.lookup bd used) of
       (Just True, Just True)   -> reifyArithChcs al bl op
       (Just True, Just False)  -> reifyArithChcs al br op
       (Just False, Just True)  -> reifyArithChcs ar bl op
       (Just False, Just False) -> reifyArithChcs ar br op
       (Just True, Nothing) -> do St.modify . second $ M.adjust (const True) ad
                                  lift $ SC.push 1
                                  clearSt
                                  resl <- reifyArithChcs al bl op
                                  bl' <- handleSBoolChc resl
                                  resL <- getResult
                                  lift $ SC.pop 1

                                  St.modify . second $ M.insert bd False

                                  lift $ SC.push 1
                                  clearSt
                                  resr <- reifyArithChcs al br op
                                  br' <- handleSBoolChc resr
                                  resR <- getResult
                                  lift $ SC.pop 1


                                  store $ VChc (dimName bd) resL resR

                                  St.modify . second $ M.delete bd
                                  return $ VChc bd (Plain bl') (Plain br')

       (Just False, Nothing) -> do St.modify . second $ M.adjust (const True) ad
                                   lift $ SC.push 1
                                   clearSt
                                   resl <- reifyArithChcs ar bl op
                                   bl' <- handleSBoolChc resl
                                   resL <- getResult
                                   lift $ SC.pop 1

                                   St.modify . second $ M.insert bd False
                                   lift $ SC.push 1
                                   clearSt
                                   resr <- reifyArithChcs ar br op
                                   br' <- handleSBoolChc resr
                                   resR <- getResult
                                   lift $ SC.pop 1

                                   store $ VChc (dimName bd) resL resR

                                   St.modify . second $ M.delete bd
                                   return $ VChc bd (Plain bl') (Plain br')

       (Nothing, Just False) -> do St.modify . second $ M.adjust (const True) ad
                                   lift $ SC.push 1
                                   clearSt
                                   resl <- reifyArithChcs al br op
                                   bl' <- handleSBoolChc resl
                                   resL <- getResult
                                   lift $ SC.pop 1

                                   St.modify . second $ M.insert bd False

                                   lift $ SC.push 1
                                   clearSt
                                   resr <- reifyArithChcs ar br op
                                   br' <- handleSBoolChc resr
                                   resR <- getResult
                                   lift $ SC.pop 1

                                   store $ VChc (dimName ad) resL resR

                                   St.modify . second $ M.delete bd
                                   return $ VChc ad (Plain bl') (Plain br')

       (Nothing, Just True) -> do St.modify . second $ M.adjust (const True) ad
                                  lift $ SC.push 1
                                  clearSt
                                  resl <- reifyArithChcs al bl op
                                  bl' <- handleSBoolChc resl
                                  lRes <- getResult
                                  lift $ SC.pop 1

                                  St.modify . second $ M.insert ad False

                                  lift $ SC.push 1
                                  clearSt
                                  resr <- reifyArithChcs ar bl op
                                  br' <- handleSBoolChc resr
                                  rRes <- getResult
                                  lift $ SC.pop 1


                                  St.modify . second $ M.delete ad
                                  store $ VChc (dimName ad) lRes rRes
                                  return $ VChc ad (Plain bl') (Plain br')

       (Nothing, Nothing) -> do St.modify . second $ M.insert bd False
                                St.modify . second $ M.insert ad False
                                St.modify . second $ M.adjust (const True) ad
                                lift $ SC.push 1
                                clearSt
                                reslr <- reifyArithChcs al br op
                                blr <- handleSBoolChc reslr
                                lrRes <- getResult
                                lift $ SC.pop 1

                                lift $ SC.push 1
                                clearSt
                                resrr <- reifyArithChcs ar br op
                                brr <- handleSBoolChc resrr
                                rrRes <- getResult
                                lift $ SC.pop 1

                                St.modify . second $ M.adjust (const True) bd
                                St.modify . second $ M.adjust (const False) ad

                                St.modify . second $ M.adjust (const True) ad
                                lift $ SC.push 1
                                clearSt
                                resll <- reifyArithChcs al bl op
                                bll <- handleSBoolChc resll
                                llRes <- getResult
                                lift $ SC.pop 1

                                lift $ SC.push 1
                                clearSt
                                resrl <- reifyArithChcs ar bl op
                                brl <- handleSBoolChc resrl
                                rlRes <- getResult
                                lift $ SC.pop 1

                                St.modify . second $ M.delete ad
                                St.modify . second $ M.delete bd

                                store $ VChc (dimName bd)
                                  (VChc (dimName ad) llRes lrRes)
                                  (VChc (dimName ad) rlRes rrRes)

                                return $ VChc bd
                                  (VChc ad (Plain bll) (Plain blr))
                                  (VChc ad (Plain brl) (Plain brr))

vSMTSolve'_ :: VIExpr a SNum -> IncVSMTSolve d (V (Dim a) SNum)
vSMTSolve'_ !(Ref RefI i) = return . Plain $ i
vSMTSolve'_ !(Ref RefD d) = return . Plain $ d
vSMTSolve'_ !(LitI (I i)) = return . Plain . SI . S.literal . fromIntegral $ i
vSMTSolve'_ !(LitI (D d)) = return . Plain . SD . S.literal $ d
vSMTSolve'_ !(OpI op e) = do e' <- vSMTSolve'_ e
                             return $ (handler op) e'
  where handler Neg  = negate
        handler Abs  = abs
        handler Sign = signum
vSMTSolve'_ !(OpII op l r) = do l' <- vSMTSolve'_ l
                                r' <- vSMTSolve'_ r
                                return $ handler op l' r'
  where handler Add  = (+)
        handler Sub  = (-)
        handler Mult = (*)
        handler Div  = (./)
        handler Mod  = (.%)
vSMTSolve'_ (ChcI d l r) = do r' <- vSMTSolve'_ r
                              l' <- vSMTSolve'_ l
                              return $ VChc d l' r'

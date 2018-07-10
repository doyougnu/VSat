module Run ( Opts (..)
           , Result (..)
           , SatDict
           , Log
           , runAD
           , runBF
           , runVS
           , runVSMT
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict    as St
import qualified Data.SBV.Internals  as I
import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC
import           Prelude hiding (LT, GT, EQ)

import Control.Arrow (first, second)

import GHC.Generics
import Control.DeepSeq               (NFData)

import Data.Maybe                    (catMaybes)

import VProp.Types
import VProp.SBV
import VProp.Core
import V

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict a = (M.Map Config Bool, M.Map a Bool) -- keys may incur perf penalty

-- | The optimizations that could be set
newtype Opts a = Opts [VProp a a -> VProp a a] -- ^ a list of optimizations


-- | Type convenience for Log
type Log = String

-- | Takes a dimension d, a value a, and a result r
type Env a r = RWST (Opts a) Log (SatDict a) IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: Opts a
_emptyOpts = Opts []

_emptySt :: SatDict a
_emptySt = (,) M.empty M.empty

-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env a r -> Opts a -> SatDict a -> IO (r, (SatDict a),  Log)
_runEnv m opts st = runRWST m opts st

-- TODO use configurate and load the config from a file
runEnv :: (VProp String String-> Env String Result)
       -> [VProp String String-> VProp String String]
       -> VProp String String-> IO (Result , (SatDict String), Log)
runEnv f !opts !x = _runEnv (f x) (Opts opts) (initSt x)

runAD :: [VProp String String -> VProp String String]
      -> VProp String String
      -> IO (Result, SatDict String, Log)
runAD = runEnv runAndDecomp

runBF :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO (Result, SatDict String, Log)
runBF = runEnv runBruteForce

runVS :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO (Result, SatDict String, Log)
runVS = runEnv runVSolve

runVSMT :: [VProp String String -> VProp String String]
  -> VProp String String
  -> IO (Result, SatDict String, Log)
runVSMT = runEnv runVSMTSolve

-- runEnvFirst :: Bool -> Bool -> Bool -> [VProp String -> VProp String] -> VProp String -> IO (V Dim (Maybe I.SMTModel))
-- runEnvFirst base bAD bOpt opts x = (head . unbox . fst') <$> _runEnv (work x) (_setOpts base bAD bOpt opts) (initSt x)
--   where fst' (y,_,_)  = y
--         unbox (Vr xs) = xs

-- | Given a VProp a term generate the satisfiability map
initSt :: (Show a, Ord a) => VProp a a -> (SatDict a)
initSt prop = (sats, vs)
  where sats = M.fromList . fmap (\x -> (x, False)) $ M.fromList <$> configs prop
        vs = M.fromSet (const False) (vars prop)


-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter [Char] m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter [Char] m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x


-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runBruteForce :: (Show a, Ord a) =>
  (MonadTrans t,
   MonadState (SatDict a) (t IO)) => VProp a a -> t IO Result
runBruteForce prop = lift $ flip evalStateT _emptySt $
  do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> sequence $! (y, selectVariant y prop)) <$> confs
  plainModels <- lift $ mapM (S.sat . symbolicPropExpr . snd) $! catMaybes plainProps
  return $ L plainModels

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (MonadTrans t, Monad (t IO)) => VProp String String -> t IO Result
runAndDecomp prop = do
  res <- lift . S.runSMT $ do
    p <- symbolicPropExpr $ (andDecomp prop dimName)
    S.constrain p
    SC.query $ do
      c <- SC.checkSat
      case c of
        SC.Unk -> error "asdf"
        SC.Unsat -> return Nothing
        SC.Sat -> do model' <- SC.getModel
                     return $ Just model'
  lift . return $ R res

runVSolve :: (MonadReader (Opts String) (t IO), MonadTrans t) =>
  VProp String String -> t IO Result
runVSolve prop =
  do opts <- ask
     (result,_) <- lift . S.runSMT . vSolve $ St.evalStateT (propToSBool prop) (M.empty, M.empty)
     lift . return . Vsolve $ result

runVSMTSolve :: (MonadTrans t, MonadReader (Opts String) (t IO)) =>
  VProp String String -> t IO Result
runVSMTSolve prop =
  do opts <- ask
     (res,_) <- lift . S.runSMT . vSMTSolve $ St.evalStateT (propToSBool prop) (M.empty, M.empty)
     lift . return $ Vsmt res

-- | main workhorse for running the SAT solver
data Result = R (Maybe I.SMTModel)
            | L [S.SatResult]
            | Vsolve [V Dim (Maybe I.SMTModel)]
            | Vsmt   [V Dim (Maybe I.SMTResult)]
            deriving (Generic)

instance NFData Result

-- work :: ( MonadTrans t
--         , MonadState (SatDict String) (t IO)
--         , MonadReader (Opts String) (t IO)) => VProp String String -> t IO Result
-- work prop = do
--   baselines <- asks runBaselines
--   bAD <- asks runAD
--   -- fix this antipattern later
--   if baselines
--     then if bAD
--          then lift $ runAndDecomp prop  >>= return . R
--          else do
--     runBruteForce prop >>= return . L
--     else do
--     opts <- asks optimizations
--     (result,_) <- lift . S.runSMT . vSolve $ St.evalStateT (propToSBool prop) (M.empty, M.empty)
--     return $ Vr result

-- | wrapper around map to keep track of the variable references we've seen, a,
-- and their symbolic type, b
type UsedVars a b = M.Map a b

-- | A state monad transformer that holds two usedvar maps, one for booleans and
-- one for doubles
type IncPack a b = St.StateT ((UsedVars a S.SBool, UsedVars a S.SDouble)) S.Symbolic b

-- | a map to keep track if a dimension has been seen before
type UsedDims a = M.Map a Bool

-- | the internal state for the incremental solve algorithm, it holds a result
-- list, and the used dims map
type IncState a = ([V Dim (Maybe a)], UsedDims Dim)

-- | the incremental solve monad, with the base monad being the query monad so
-- we can pull out sbv models Hardcoding so that I don't have to write the mtl
-- typeclass. I do not expect these to change much
type IncVSolve a    = St.StateT (IncState I.SMTModel)  SC.Query a
type IncVSMTSolve a = St.StateT (IncState I.SMTResult) SC.Query a

-- | Given a VProp with references at the boolean level as SBools, and at the
-- number level as SDoubles, recur through the proposition loading terms into
-- SBV. When we hit a choice we manipulate the assertion stack to maximize reuse
-- of non-variational terms and then cons the resultant model for each branch of
-- the choice onto the result list.
vSolve :: S.Symbolic (VProp S.SBool S.SDouble) -> S.Symbolic (IncState I.SMTModel)
vSolve prop = do prop' <- prop
                 SC.query $ St.execStateT (vSolve_ prop') ([], M.empty)

-- | Solve a VSMT proposition
vSMTSolve :: S.Symbolic (VProp S.SBool S.SDouble) -> S.Symbolic (IncState I.SMTResult)
vSMTSolve prop = do prop' <- prop
                    SC.query $ St.execStateT (vSMTSolve_ prop') ([], M.empty)

-- | This ensures two things: 1st we need all variables to be symbolic before
-- starting query mode. 2nd we cannot allow any duplicates to be called on a
-- string -> symbolic a function or missiles will launch.
propToSBool :: VProp String String -> IncPack String (VProp S.SBool S.SDouble)
propToSBool = bitraverse smtBool smtDouble

-- | convert every reference to a boolean, keeping track of what you've seen before
smtBool :: String -> IncPack String S.SBool
smtBool str = do (st,_) <- get
                 case str `M.lookup` st of
                   Nothing -> do b <- lift $ S.sBool str
                                 St.modify (first $ M.insert str b)
                                 return b
                   Just x  -> return x

-- | convert every reference to a double, keeping track of what you've seen before
smtDouble :: String -> IncPack String S.SDouble
smtDouble str = do (_,st) <- get
                   case str `M.lookup` st of
                     Nothing -> do b <- lift $ S.sDouble str
                                   St.modify (second $ M.insert str b)
                                   return b
                     Just x  -> return x

-- | get a model out given an S.SBool
-- getModel :: SC.Query (V Dim (Maybe I.SMTModel))
getModel_ :: SC.Query a -> SC.Query (V d (Maybe a))
getModel_ f = do cs <- SC.checkSat
                 case cs of
                   SC.Unk   -> error "Unknown!"
                   SC.Unsat -> return (Plain Nothing)
                   SC.Sat   -> (Plain . Just) <$> f

getVModel :: SC.Query (V d (Maybe I.SMTModel))
getVModel = getModel_ SC.getModel
getVSMTModel :: SC.Query (V d (Maybe S.SMTResult))
getVSMTModel = getModel_ SC.getSMTResult

-- | type class needed to avoid lifting for constraints in the IncSolve monad
instance (Monad m, I.SolverContext m) =>
  I.SolverContext (StateT (IncState a) m) where
  constrain = lift . S.constrain
  namedConstraint = (lift .) . S.namedConstraint
  setOption = lift . S.setOption

-- | Helper functoins for the n-ary cases
vSolveHelper :: S.SBool -> [VProp S.SBool S.SDouble] ->
  (S.SBool -> S.SBool -> S.SBool) -> IncVSolve S.SBool
vSolveHelper acc ![]     _ = return acc
vSolveHelper acc !(x:xs) f = do b <- vSolve_ x; vSolveHelper (b `f` acc) xs f

vSMTSolveHelper :: S.SBool -> [VProp S.SBool S.SDouble] ->
  (S.SBool -> S.SBool -> S.SBool) -> IncVSMTSolve S.SBool
vSMTSolveHelper acc ![]     _ = return acc
vSMTSolveHelper acc !(x:xs) f = do b <- vSMTSolve_ x; vSMTSolveHelper (b `f` acc) xs f

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl
vSMTSolve_ :: VProp S.SBool S.SDouble -> IncVSMTSolve S.SBool
vSMTSolve_ (RefB b) = return b
vSMTSolve_ (LitB b) = return $ S.literal b
vSMTSolve_ (OpB Not bs)= do b <- vSMTSolve_ (S.bnot bs)
                            S.constrain b
                            return b
vSMTSolve_ (OpBB op l r) = do bl <- vSMTSolve_ l
                              br <- vSMTSolve_ r
                              let op' = handler op
                              S.constrain $ bl `op'` br
                              return $ bl `op'` br
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
vSMTSolve_ (OpIB op l r) = do bl <- vSMTSolve'_ l
                              br <- vSMTSolve'_ r
                              let op' = handler op
                              S.constrain $ bl `op'` br
                              return $ bl `op'` br
  where handler LT  = (.<)
        handler LTE = (.<=)
        handler GTE = (.>=)
        handler GT  = (.>)
        handler EQ  = (.==)
        handler NEQ = (./=)
vSMTSolve_ (Opn And ps) = do b <- vSMTSolveHelper S.true ps (S.&&&)
                             S.constrain b
                             return b
vSMTSolve_ (Opn Or ps) = do b <- vSMTSolveHelper S.true ps (S.|||)
                            S.constrain b
                            return b
vSMTSolve_ (ChcB d l r) = {-# SCC "Choice_Solve"#-}
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> vSMTSolve_ l
       Just False -> vSMTSolve_ r
       Nothing    -> do St.modify . second $ M.insert d True
                        lift $ SC.push 1
                        _ <- vSMTSolve_ l
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const False) d
                        lift $ SC.push 1
                        b <- vSMTSolve_ r
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc d lmodel rmodel))

                        St.modify . second $ M.delete d
                        return b

-- | The incremental solve algorithm just for VIExprs
vSMTSolve'_ :: VIExpr S.SDouble -> IncVSMTSolve S.SDouble
vSMTSolve'_ (RefI i) = return i
vSMTSolve'_ (LitI (I i)) = return . S.literal . fromIntegral $ i
vSMTSolve'_ (LitI (D d)) = return . S.literal $ d
vSMTSolve'_ (OpI op e) = do e' <- vSMTSolve'_ e
                            return $ (handler op) e'
  where handler Neg  = negate
        handler Abs  = abs
        handler Sign = signum
vSMTSolve'_ (OpII op l r) = do l' <- vSMTSolve'_ l
                               r' <- vSMTSolve'_ r
                               return $ handler op l' r'
  where handler Add  = (+)
        handler Sub  = (-)
        handler Mult = (*)
        handler Div  = (./)
        handler Mod  = (.%)
vSMTSolve'_ (ChcI d l r) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> vSMTSolve'_ l
       Just False -> vSMTSolve'_ r
       Nothing    -> do St.modify . second $ M.insert d True
                        lift $ SC.push 1
                        _ <- vSMTSolve'_ l
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const False) d
                        lift $ SC.push 1
                        b <- vSMTSolve'_ r
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc d lmodel rmodel))

                        St.modify . second $ M.delete d
                        return b

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl
vSolve_ :: VProp S.SBool S.SDouble -> IncVSolve S.SBool
vSolve_ (RefB b) = return b
vSolve_ (LitB b) = return $ S.literal b
vSolve_ (OpB Not bs)= do b <- vSolve_ (S.bnot bs)
                         S.constrain b
                         return b
vSolve_ (OpBB op l r) = do bl <- vSolve_ l
                           br <- vSolve_ r
                           let op' = handler op
                           S.constrain $ bl `op'` br
                           return $ bl `op'` br
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
vSolve_ (OpIB _ _ _) = error "You called the SAT Solver with SMT Exprs! Launching the missiles like you asked!"
vSolve_ (Opn And ps) = do b <- vSolveHelper S.true ps (S.&&&)
                          S.constrain b
                          return b
vSolve_ (Opn Or ps) = do b <- vSolveHelper S.true ps (S.|||)
                         S.constrain b
                         return b
vSolve_ (ChcB d l r) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> vSolve_ l
       Just False -> vSolve_ r
       Nothing    -> do St.modify . second $ M.insert d True
                        lift $ SC.push 1
                        _ <- vSolve_ l
                        lmodel <- lift $ getVModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const False) d
                        lift $ SC.push 1
                        b <- vSolve_ r
                        rmodel <- lift $ getVModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc d lmodel rmodel))

                        St.modify . second $ M.delete d
                        return b

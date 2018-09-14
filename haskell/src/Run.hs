module Run ( Result (..)
           , SatDict
           , Log
           , runAD
           , runBF
           , runVS
           , runVSMT
           , unbox
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

import Control.Arrow (first, second)

import GHC.Generics
import Control.DeepSeq               (NFData)

import Data.Maybe                    (catMaybes)

import VProp.Types
import VProp.SBV
import VProp.Core
import V
import Utils
import Config

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict a = (M.Map Config Bool, M.Map a Bool) -- keys may incur perf penalty

-- | Type convenience for Log
type Log = String

-- | Takes a dimension d, a value a, and a result r
type Env a r = RWST (SMTConf a) Log (SatDict a) IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptySt :: SatDict a
_emptySt = (,) M.empty M.empty

-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env a r -> SMTConf a -> SatDict a -> IO (r, (SatDict a),  Log)
_runEnv m opts st = runRWST m opts st

-- TODO use configurate and load the config from a file
runEnv :: (VProp String String-> Env String Result)
       -> SMTConf String
       -> VProp String String-> IO (Result, (SatDict String), Log)
runEnv f conf !x = _runEnv (f x) conf (initSt x)

runAD :: SMTConf String
      -> VProp String String
      -> IO [V String (Maybe S.SatResult)]
runAD os p = fmap (bimap id (fmap S.SatResult)) . unbox . fst' <$>
             runEnv runAndDecomp os p

-- | Run the brute force solver
runBF :: SMTConf String
  -> VProp String String
  -> IO (Result, SatDict String, Log)
runBF = runEnv runBruteForce

-- | Run the variational sat solver given a list of optimizations and a prop.
-- This can throw an exception if the prop has SMT terms in it
runVS :: SMTConf String
  -> VProp String String
  -> IO (Result, SatDict String, Log)
runVS = runEnv runVSolve

-- | Run the VSMT solver given a list of optimizations and a prop
runVSMT :: SMTConf String
  -> VProp String String
  -> IO (Result, SatDict String, Log)
runVSMT = runEnv runVSMTSolve

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
  (MonadTrans t, MonadState (SatDict a) (t IO)) => VProp a a -> t IO Result
runBruteForce prop = lift $ flip evalStateT _emptySt $
  do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> sequence $! (y, selectVariant y prop)) <$> confs
  plainMs <- lift $ mapM (S.sat . symbolicPropExpr . snd) $! catMaybes plainProps
  return $ L plainMs

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (MonadTrans t, Monad (t IO)) => VProp String String -> t IO Result
runAndDecomp prop = do
  res <- lift . S.runSMT $ do
    p <- symbolicPropExpr $ andDecomp prop dimName
    SC.query $ do S.constrain p; getVSMTModel
  lift . return $ V [res]

runVSolve :: (MonadReader (SMTConf String) (t IO), MonadTrans t) =>
  VProp String String -> t IO Result
runVSolve prop =
  do cnf <- ask
     let prop' = foldr' ($!) prop (opts cnf)
     (result,_) <- lift . S.runSMTWith (conf cnf) . vSolve $
                   St.evalStateT (propToSBool prop') (M.empty, M.empty)
     lift . return . V $ result

runVSMTSolve :: (MonadTrans t, MonadReader (SMTConf String) (t IO)) =>
  VProp String String -> t IO Result
runVSMTSolve prop =
  do cnf <- ask
     let prop' = foldr' ($!) prop (opts cnf)
     (res,_) <- lift . S.runSMTWith (conf cnf) . vSMTSolve $
       St.evalStateT (propToSBool prop') (M.empty, M.empty)
     lift . return . V $ res

-- | main workhorse for running the SAT solver
data Result = L [S.SatResult]
            | V [V String (Maybe S.SMTResult)]
            deriving (Generic)

-- | unbox a result to get the SMTResults
unbox :: Result -> [V String (Maybe S.SMTResult)]
unbox (L _) = []
unbox (V xs) = xs

instance NFData Result

-- | wrapper around map to keep track of the variable references we've seen, a,
-- and their symbolic type, b
type UsedVars a b = M.Map a b

-- | A state monad transformer that holds two usedvar maps, one for booleans and
-- one for doubles
type IncPack a b = St.StateT ((UsedVars a S.SBool, UsedVars a SNum)) S.Symbolic b

-- | a map to keep track if a dimension has been seen before
type UsedDims a = M.Map a Bool

-- | the internal state for the incremental solve algorithm, it holds a result
-- list, and the used dims map
type IncState a = ([V String (Maybe a)], UsedDims Dim)

-- | the incremental solve monad, with the base monad being the query monad so
-- we can pull out sbv models Hardcoding so that I don't have to write the mtl
-- typeclass. I do not expect these to change much
type IncVSolve a    = St.StateT (IncState S.SMTResult) SC.Query a
type IncVSMTSolve a = St.StateT (IncState S.SMTResult) SC.Query a

-- | Given a VProp with references at the boolean level as SBools, and at the
-- number level as SDoubles, recur through the proposition loading terms into
-- SBV. When we hit a choice we manipulate the assertion stack to maximize reuse
-- of non-variational terms and then cons the resultant model for each branch of
-- the choice onto the result list.
vSolve :: S.Symbolic (VProp S.SBool SNum) -> S.Symbolic (IncState S.SMTResult)
vSolve prop = do prop' <- prop
                 SC.query $
                   do res <- St.execStateT (vSolve_ prop') ([], M.empty)
                      return res

-- | Solve a VSMT proposition
vSMTSolve :: S.Symbolic (VProp S.SBool SNum) -> S.Symbolic (IncState S.SMTResult)
vSMTSolve prop = do prop' <- prop
                    SC.query $
                      do
                      res' <- St.execStateT (vSMTSolve_ prop') ([], M.empty)
                      -- res <-
                      --   if isPlain prop'
                      --   then do
                      --     prf <- SC.getSMTResult
                      --     return $ first ((:) (Plain . Just $ prf)) res'
                      --   else return res'
                      return res'

-- | This ensures two things: 1st we need all variables to be symbolic before
-- starting query mode. 2nd we cannot allow any duplicates to be called on a
-- string -> symbolic a function or missiles will launch.
propToSBool :: VProp String String -> IncPack String (VProp S.SBool SNum)
propToSBool !(RefB x)     = RefB   <$> smtBool x
propToSBool !(OpB o e)    = OpB  o <$> propToSBool e
propToSBool !(OpBB o l r) = OpBB o <$> propToSBool l <*> propToSBool r
propToSBool !(Opn o xs)   = Opn  o <$> traverse propToSBool xs
propToSBool !(ChcB d l r) = ChcB d <$> propToSBool l <*> propToSBool r
propToSBool !(OpIB o l r) = OpIB o <$> propToSBool' l <*> propToSBool' r
propToSBool !(LitB b)     = return $ LitB b

propToSBool' :: VIExpr String -> IncPack String (VIExpr SNum)
propToSBool' !(Ref RefI i) = Ref RefI <$> smtInt i
propToSBool' !(Ref RefD d) = Ref RefD <$> smtDouble d
propToSBool' !(OpI o e)    = OpI o    <$> propToSBool' e
propToSBool' !(OpII o l r) = OpII o <$> propToSBool' l <*> propToSBool' r
propToSBool' !(ChcI d l r) = ChcI d <$> propToSBool' l <*> propToSBool' r
propToSBool' !(LitI x)     = return $ LitI x

-- | convert every reference to a boolean, keeping track of what you've seen
-- before
smtBool :: String -> IncPack String S.SBool
smtBool str = do (st,_) <- get
                 case str `M.lookup` st of
                   Nothing -> do b <- lift $ S.sBool str
                                 St.modify (first $ M.insert str b)
                                 return b
                   Just x  -> return x

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtInt :: String -> IncPack String SNum
smtInt str = do (_,st) <- get
                case str `M.lookup` st of
                  Nothing -> do b <- lift $ S.sInt64 str
                                let b' = SI b
                                St.modify (second $ M.insert str b')
                                return b'
                  Just x  -> return x

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtDouble :: String -> IncPack String SNum
smtDouble str = do (_,st) <- get
                   case str `M.lookup` st of
                     Nothing -> do b <- lift $ S.sDouble str
                                   let b' = SD b
                                   St.modify (second $ M.insert str b')
                                   return b'
                     Just x  -> return x

getVSMTModel :: SC.Query (V d (Maybe S.SMTResult))
getVSMTModel = do cs <- SC.checkSat
                  case cs of
                    SC.Unk   -> error "Unknown!"
                    SC.Unsat -> return (Plain Nothing)
                    SC.Sat   -> (Plain . Just) <$> SC.getSMTResult

-- | type class needed to avoid lifting for constraints in the IncSolve monad
instance (Monad m, I.SolverContext m) =>
  I.SolverContext (StateT (IncState a) m) where
  constrain = lift . S.constrain
  namedConstraint = (lift .) . S.namedConstraint
  setOption = lift . S.setOption


-- | Helper functoins for the n-ary cases
vSolveHelper :: S.SBool -> [VProp S.SBool SNum] ->
  (S.SBool -> S.SBool -> S.SBool) -> IncVSolve S.SBool
vSolveHelper !acc ![]     _ = return acc
vSolveHelper !acc !(x:xs) f = do b <- vSolve_ x; vSolveHelper (b `f` acc) xs f

vSMTSolveHelper :: S.SBool -> [VProp S.SBool SNum] ->
  (S.SBool -> S.SBool -> S.SBool) -> IncVSMTSolve S.SBool
vSMTSolveHelper !acc ![]     _ = return acc
vSMTSolveHelper !acc !(x:xs) f = do b <- vSMTSolve_ x
                                    vSMTSolveHelper (b `f` acc) xs f

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl
vSMTSolve_ :: VProp S.SBool SNum -> IncVSMTSolve S.SBool
vSMTSolve_ !(RefB b) = return b
vSMTSolve_ !(LitB b) = return $ S.literal b
vSMTSolve_ !(OpB Not bs)= do b <- vSMTSolve_ bs
                             S.constrain $ S.bnot b
                             return b
vSMTSolve_ !(OpBB op l r) = do br <- vSMTSolve_ r
                               bl <- vSMTSolve_ l
                               let op' = handler op
                               S.constrain $ bl `op'` br
                               return $ bl `op'` br
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
vSMTSolve_ !(OpIB op l r) = do l' <- vSMTSolve'_ l
                               r' <- vSMTSolve'_ r
                               test l' r' (handler op)
                               return true
  where handler LT  = (.<)
        handler LTE = (.<=)
        handler GTE = (.>=)
        handler GT  = (.>)
        handler EQ  = (.==)
        handler NEQ = (./=)
vSMTSolve_ !(Opn And ps) = do b <- vSMTSolveHelper S.true ps (&&&)
                              S.constrain b
                              return b
vSMTSolve_ !(Opn Or ps) = do b <- vSMTSolveHelper S.true ps (|||)
                             S.constrain b
                             return b
vSMTSolve_ !(ChcB d l r) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> vSMTSolve_ l
       Just False -> vSMTSolve_ r
       Nothing    -> do St.modify . second $ M.insert d False
                        lift $ SC.push 1
                        r' <- vSMTSolve_ r
                        S.constrain r'
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const True) d
                        lift $ SC.push 1
                        l' <- vSMTSolve_ l
                        S.constrain l'
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc (dimName d) lmodel rmodel))

                        St.modify . second $ M.delete d
     -- this return statement should never matter because we've reset the
     -- assertion stack. So I just return r' here to fulfill the type
                        return r'
test' :: V Dim S.SBool -> IncVSMTSolve S.SBool
test' (Plain a) = S.constrain a >> return a
test' (VChc d l r) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> do l' <- test' l
                        S.constrain l'
                        return l'
       Just False -> do r' <- test' r
                        S.constrain r'
                        return r'

       Nothing    -> do St.modify . second $ M.insert d False
                        lift $ SC.push 1
                        br <- test' r
                        S.constrain br
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const True) d
                        lift $ SC.push 1
                        bl <-test' l
                        S.constrain bl
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc (dimName d) lmodel rmodel))

                        St.modify . second $ M.delete d

                        return bl

test :: V Dim SNum -> V Dim SNum -> (SNum -> SNum -> S.SBool)
  -> IncVSMTSolve (V Dim S.SBool)
test (Plain a) (Plain b) op = return . Plain $ a `op` b
test (Plain a) (VChc d l r) op =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> return $ l >>= return . op a
       Just False -> return $ r >>= return . op a
       Nothing    -> do St.modify . second $ M.insert d False
                        lift $ SC.push 1
                        let br = op a <$> r
                        _ <- test' br
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const True) d
                        lift $ SC.push 1
                        let bl = op a <$> l
                        _ <- test' bl
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc (dimName d) lmodel rmodel))

                        St.modify . second $ M.delete d
                        return bl
test (VChc d l r) (Plain a) op =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> return $ l >>= return . flip op a
       Just False -> return $ r >>= return . flip op a
       Nothing    -> do St.modify . second $ M.insert d False
                        lift $ SC.push 1
                        let br = op a <$> r
                        _ <- test' br
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const True) d
                        lift $ SC.push 1
                        let bl = op a <$> l
                        _ <- test' bl
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc (dimName d) lmodel rmodel))

                        St.modify . second $ M.delete d
                        return bl

test (VChc ad al ar) (VChc bd bl br) op =
  do (_, used) <- get
     case (M.lookup ad used, M.lookup bd used) of
       (Just True, Just True)   -> test al bl op
       (Just True, Just False)  -> test al br op
       (Just False, Just True)  -> test ar bl op
       (Just False, Just False) -> test ar br op
       (Just True, Nothing) -> do
                                  St.modify . second $ M.insert bd False
                                  lift $ SC.push 1
                                  resr <- test al br op
                                  br' <- test' resr
                                  S.constrain br'
                                  rmodel <- lift $ getVSMTModel
                                  lift $ SC.pop 1

                                  St.modify . second $ M.adjust (const True) ad
                                  lift $ SC.push 1
                                  resl <- test al bl op
                                  bl' <- test' resl
                                  S.constrain bl'
                                  lmodel <- lift $ getVSMTModel
                                  lift $ SC.pop 1

                                  St.modify . first $ ((:) (VChc (dimName bd) lmodel rmodel))

                                  St.modify . second $ M.delete bd
                                  return resl

       (Just False, Nothing) -> do
                                   St.modify . second $ M.insert bd False
                                   lift $ SC.push 1
                                   resr <- test ar br op
                                   br' <- test' resr
                                   S.constrain br'
                                   rmodel <- lift $ getVSMTModel
                                   lift $ SC.pop 1

                                   St.modify . second $ M.adjust (const True) ad
                                   lift $ SC.push 1
                                   resl <- test ar bl op
                                   bl' <- test' resr
                                   S.constrain bl'
                                   lmodel <- lift $ getVSMTModel
                                   lift $ SC.pop 1

                                   St.modify . first $ ((:) (VChc (dimName bd) lmodel rmodel))

                                   St.modify . second $ M.delete bd
                                   return resl

       (Nothing, Just False) -> do
                                   St.modify . second $ M.insert ad False
                                   lift $ SC.push 1
                                   resr <- test ar br op
                                   br' <- test' resr
                                   S.constrain br'
                                   rmodel <- lift $ getVSMTModel
                                   lift $ SC.pop 1

                                   St.modify . second $ M.adjust (const True) ad
                                   lift $ SC.push 1
                                   resl <- test al bl op
                                   bl' <- test' resl
                                   S.constrain bl'
                                   lmodel <- lift $ getVSMTModel
                                   lift $ SC.pop 1

                                   St.modify . first $ ((:) (VChc (dimName ad) lmodel rmodel))

                                   St.modify . second $ M.delete ad
                                   return resl

       (Nothing, Just True) -> do
                                  St.modify . second $ M.insert ad False
                                  lift $ SC.push 1
                                  resr <- test ar bl op
                                  br' <- test' resr
                                  S.constrain br'
                                  rmodel <- lift $ getVSMTModel
                                  lift $ SC.pop 1

                                  St.modify . second $ M.adjust (const True) ad
                                  lift $ SC.push 1
                                  resl <- test al bl op
                                  bl' <- test' resl
                                  S.constrain bl'
                                  lmodel <- lift $ getVSMTModel
                                  lift $ SC.pop 1

                                  St.modify . first $ ((:) (VChc (dimName ad) lmodel rmodel))

                                  St.modify . second $ M.delete ad
                                  return resl

       (Nothing, Nothing) -> do St.modify . second $ M.insert bd False
                                lift $ SC.push 1

                                St.modify . second $ M.insert ad False
                                lift $ SC.push 1
                                resrr <- test ar br op
                                brr <- test' resrr
                                S.constrain brr
                                rrmodel <- lift $ getVSMTModel
                                lift $ SC.pop 1

                                St.modify . second $ M.adjust (const True) ad
                                lift $ SC.push 1
                                reslr <- test al br op
                                blr <- test' reslr
                                S.constrain blr
                                lrmodel <- lift $ getVSMTModel
                                lift $ SC.pop 1

                                St.modify . second $ M.adjust (const True) bd
                                lift $ SC.push 1

                                St.modify . second $ M.insert ad False
                                lift $ SC.push 1
                                resrl <- test ar bl op
                                brl <- test' resrl
                                S.constrain brl
                                rlmodel <- lift $ getVSMTModel
                                lift $ SC.pop 1

                                St.modify . second $ M.adjust (const True) ad
                                lift $ SC.push 1
                                resll <- test al bl op
                                bll <- test' resll
                                S.constrain bll
                                llmodel <- lift $ getVSMTModel
                                lift $ SC.pop 1

                                St.modify . second $ M.delete ad
                                St.modify . second $ M.delete bd
                                St.modify . first $ ((:) (VChc (dimName bd)
                                                          (VChc (dimName ad) llmodel lrmodel)
                                                          (VChc (dimName ad) rlmodel rrmodel)))
                                return resll

-- -- | The incremental solve algorithm just for VIExprs
-- vSMTSolve''_ :: VIExpr SNum -> VIExpr SNum ->
--   (SNum -> SNum -> S.SBool) -> IncVSMTSolve S.SBool
-- vSMTSolve''_ !(Ref _ l) !(Ref _ r) op = do S.constrain $ l `op` r
--                                            return $ l `op` r

-- vSMTSolve''_ !(Ref _ l) !(LitI (I r)) op = do S.constrain $ l `op` r'
--                                               return $ l `op` r'
--   where r' = iToSNum r

-- vSMTSolve''_ !(Ref _ l) !(LitI (D r)) op = do S.constrain $ l `op` r'
--                                               return $ l `op` r'
--   where r' = dToSNum r

-- vSMTSolve''_ !(LitI (I l)) !(Ref _ r) op = do S.constrain $ l' `op` r
--                                               return $ l' `op` r
--   where l' = iToSNum l

-- vSMTSolve''_ !(LitI (D l)) !(Ref _ r) op = do S.constrain $ l' `op` r
--                                               return $ l' `op` r
--   where l' = dToSNum l

-- vSMTSolve''_ !(LitI (I l)) !(LitI (I r)) op = do S.constrain $ l' `op` r'
--                                                  return $ l' `op` r'
--   where l' = iToSNum l
--         r' = iToSNum r
-- vSMTSolve''_ !(LitI (I l)) !(LitI (D r)) op = do S.constrain $ l' `op` r'
--                                                  return $ l' `op` r'
--   where l' = iToSNum l
--         r' = dToSNum r
-- vSMTSolve''_ !(LitI (D l)) !(LitI (I r)) op = do S.constrain $ l' `op` r'
--                                                  return $ l' `op` r'
--   where l' = dToSNum l
--         r' = iToSNum r
-- vSMTSolve''_ !(LitI (D l)) !(LitI (D r)) op = do S.constrain $ l' `op` r'
--                                                  return $ l' `op` r'
--   where l' = dToSNum l
--         r' = dToSNum r

-- vSMTSolve''_ !(ChcI ad al ar) (ChcI bd bl br) op =
--   do (_, used) <- get
--      case (M.lookup ad used, M.lookup bd used) of
--        (Just True, Just True)   -> vSMTSolve''_ al bl op
--        (Just True, Just False)  -> vSMTSolve''_ al br op
--        (Just False, Just True)  -> vSMTSolve''_ ar bl op
--        (Just False, Just False) -> vSMTSolve''_ ar br op
--        (Just True, Nothing) -> do
--                                   St.modify . second $ M.insert bd False
--                                   lift $ SC.push 1
--                                   b <- vSMTSolve''_ al br op
--                                   S.constrain b
--                                   rmodel <- lift $ getVSMTModel
--                                   lift $ SC.pop 1

--                                   St.modify . second $ M.adjust (const True) ad
--                                   lift $ SC.push 1
--                                   b' <- vSMTSolve''_ al bl op
--                                   S.constrain b'
--                                   lmodel <- lift $ getVSMTModel
--                                   lift $ SC.pop 1

--                                   St.modify . first $ ((:) (VChc (dimName bd) lmodel rmodel))

--                                   St.modify . second $ M.delete bd
--                                   return b'

--        (Just False, Nothing) -> do
--                                    St.modify . second $ M.insert bd False
--                                    lift $ SC.push 1
--                                    b <- vSMTSolve''_ ar br op
--                                    S.constrain b
--                                    rmodel <- lift $ getVSMTModel
--                                    lift $ SC.pop 1

--                                    St.modify . second $ M.adjust (const True) ad
--                                    lift $ SC.push 1
--                                    b' <- vSMTSolve''_ ar bl op
--                                    S.constrain b'
--                                    lmodel <- lift $ getVSMTModel
--                                    lift $ SC.pop 1

--                                    St.modify . first $ ((:) (VChc (dimName bd) lmodel rmodel))

--                                    St.modify . second $ M.delete bd
--                                    return b'

--        (Nothing, Just False) -> do
--                                    St.modify . second $ M.insert ad False
--                                    lift $ SC.push 1
--                                    b <- vSMTSolve''_ ar br op
--                                    S.constrain b
--                                    rmodel <- lift $ getVSMTModel
--                                    lift $ SC.pop 1

--                                    St.modify . second $ M.adjust (const True) ad
--                                    lift $ SC.push 1
--                                    b' <- vSMTSolve''_ al br op
--                                    S.constrain b'
--                                    lmodel <- lift $ getVSMTModel
--                                    lift $ SC.pop 1

--                                    St.modify . first $ ((:) (VChc (dimName ad) lmodel rmodel))

--                                    St.modify . second $ M.delete ad
--                                    return b'

--        (Nothing, Just True) -> do
--                                   St.modify . second $ M.insert ad False
--                                   lift $ SC.push 1
--                                   b <- vSMTSolve''_ ar bl op
--                                   S.constrain b
--                                   rmodel <- lift $ getVSMTModel
--                                   lift $ SC.pop 1

--                                   St.modify . second $ M.adjust (const True) ad
--                                   lift $ SC.push 1
--                                   b' <- vSMTSolve''_ al bl op
--                                   S.constrain b'
--                                   lmodel <- lift $ getVSMTModel
--                                   lift $ SC.pop 1

--                                   St.modify . first $ ((:) (VChc (dimName ad) lmodel rmodel))

--                                   St.modify . second $ M.delete ad
--                                   return b'

--        (Nothing, Nothing) -> do St.modify . second $ M.insert bd False
--                                 lift $ SC.push 1

--                                 St.modify . second $ M.insert ad False
--                                 lift $ SC.push 1
--                                 brr' <- vSMTSolve''_ ar br op
--                                 S.constrain brr'
--                                 rrmodel <- lift $ getVSMTModel
--                                 lift $ SC.pop 1

--                                 St.modify . second $ M.adjust (const True) ad
--                                 lift $ SC.push 1
--                                 blr' <- vSMTSolve''_ al br op
--                                 S.constrain blr'
--                                 lrmodel <- lift $ getVSMTModel
--                                 lift $ SC.pop 1

--                                 St.modify . second $ M.adjust (const True) bd
--                                 lift $ SC.push 1

--                                 St.modify . second $ M.insert ad False
--                                 lift $ SC.push 1
--                                 brl' <- vSMTSolve''_ ar bl op
--                                 S.constrain brl'
--                                 rlmodel <- lift $ getVSMTModel
--                                 lift $ SC.pop 1

--                                 St.modify . second $ M.adjust (const True) ad
--                                 lift $ SC.push 1
--                                 bll' <- vSMTSolve''_ al bl op
--                                 S.constrain bll'
--                                 llmodel <- lift $ getVSMTModel
--                                 lift $ SC.pop 1

--                                 St.modify . second $ M.delete ad
--                                 St.modify . second $ M.delete bd
--                                 St.modify . first $ ((:) (VChc (dimName bd)
--                                                           (VChc (dimName ad) llmodel lrmodel)
--                                                           (VChc (dimName ad) rlmodel rrmodel)))
--                                 return bll'
-- vSMTSolve''_ !(ChcI ad al ar) x op =
--   do (_, used) <- get
--      case (M.lookup ad used) of
--        Just False -> vSMTSolve''_ ar x op
--        Just True  -> vSMTSolve''_ al x op
--        Nothing -> do St.modify . second $ M.insert ad False
--                      lift $ SC.push 1
--                      b <- vSMTSolve''_ ar x op
--                      S.constrain b
--                      rmodel <- lift $ getVSMTModel
--                      lift $ SC.pop 1

--                      St.modify . second $ M.adjust (const True) ad
--                      lift $ SC.push 1
--                      b' <- vSMTSolve''_ al x op
--                      S.constrain b'
--                      lmodel <- lift $ getVSMTModel
--                      lift $ SC.pop 1

--                      St.modify . first $ ((:) (VChc (dimName ad) lmodel rmodel))

--                      St.modify . second $ M.delete ad
--                      return b'

-- vSMTSolve''_ x !(ChcI ad al ar) op =
--   do (_, used) <- get
--      case (M.lookup ad used) of
--        Just False -> vSMTSolve''_ x ar op
--        Just True  -> vSMTSolve''_ x al op
--        Nothing -> do St.modify . second $ M.insert ad False
--                      lift $ SC.push 1
--                      b <- vSMTSolve''_ x ar op
--                      S.constrain b
--                      rmodel <- lift $ getVSMTModel
--                      lift $ SC.pop 1

--                      St.modify . second $ M.adjust (const True) ad
--                      lift $ SC.push 1
--                      b' <- vSMTSolve''_ x al op
--                      S.constrain b'
--                      lmodel <- lift $ getVSMTModel
--                      lift $ SC.pop 1

--                      St.modify . first $ ((:) (VChc (dimName ad) lmodel rmodel))

--                      St.modify . second $ M.delete ad
--                      return b'

-- vSMTSolve''_ x !(OpI op' e) op = vSMTSolve''_ x e' op
--   where handler Neg  = negate
--         handler Abs  = abs
--         handler Sign = signum
--         e' = handler op' <$> e

-- vSMTSolve''_ a@(OpI op' e) x op = trace (show a) $ vSMTSolve''_  e' x op
--   where handler Neg  = negate
--         handler Abs  = abs
--         handler Sign = signum
--         e' = (handler op') <$> e

-- vSMTSolve''_ (OpII op' (LitI (I l)) (LitI (I r))) x op =
--   vSMTSolve''_ (LitI (I e')) x op
--   where handler Add  = (+)
--         handler Sub  = (-)
--         handler Mult = (*)
--         handler Div  = (./)
--         handler Mod  = (.%)
--         e' = (handler op') l r

-- vSMTSolve''_ x (OpII op' (LitI (I l)) (LitI (I r))) op =
--   vSMTSolve''_ x (LitI (I e')) op
--   where handler Add  = (+)
--         handler Sub  = (-)
--         handler Mult = (*)
--         handler Div  = (./)
--         handler Mod  = (.%)
--         e' = (handler op') l r

vSMTSolve'_ :: VIExpr SNum -> IncVSMTSolve (V Dim SNum)
vSMTSolve'_ !(Ref RefI i) = return . Plain $ i
vSMTSolve'_ !(Ref RefD d) = return . Plain $ d
vSMTSolve'_ !(LitI (I i)) = return . Plain . SI . S.literal . fromIntegral $ i
vSMTSolve'_ !(LitI (D d)) = return . Plain . SD . S.literal $ d
vSMTSolve'_ !(OpI op e) = do e' <- vSMTSolve'_ e
                             return $ (handler op) e'
  where handler Neg  = negate
        handler Abs  = abs
        handler Sign = signum
vSMTSolve'_ !(OpII op l r) = do r' <- vSMTSolve'_ r
                                l' <- vSMTSolve'_ l
                                return $ handler op l' r'
  where handler Add  = (+)
        handler Sub  = (-)
        handler Mult = (*)
        handler Div  = (./)
        handler Mod  = (.%)
vSMTSolve'_ (ChcI d l r) = do l' <- vSMTSolve'_ l
                              r' <- vSMTSolve'_ r
                              return $ VChc d l' r'

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl
vSolve_ :: VProp S.SBool SNum -> IncVSolve S.SBool
vSolve_ !(RefB b) = return b
vSolve_ !(LitB b) = return $ S.literal b
vSolve_ !(OpB Not bs)= do b <- vSolve_ (S.bnot bs)
                          S.constrain b
                          return b
vSolve_ !(OpBB op l r) = do bl <- vSolve_ l
                            br <- vSolve_ r
                            let op' = handler op
                                res = bl `op'` br
                            S.constrain res
                            return $ res
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
vSolve_ !(OpIB _ _ _) = error "You called the SAT Solver with SMT Exprs! Launching the missiles like you asked!"
vSolve_ !(Opn And ps) = do b <- vSolveHelper S.true ps (&&&)
                           S.constrain b
                           return b
vSolve_ !(Opn Or ps) = do b <- vSolveHelper S.false ps (|||)
                          S.constrain b
                          return b
vSolve_ !(ChcB d l r) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> vSolve_ l
       Just False -> vSolve_ r
       Nothing    -> do St.modify . second $ M.insert d True
                        lift $ SC.push 1
                        _ <- vSolve_ l
                        lmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . second $ M.adjust (const False) d
                        lift $ SC.push 1
                        b <- vSolve_ r
                        rmodel <- lift $ getVSMTModel
                        lift $ SC.pop 1

                        St.modify . first $ ((:) (VChc (dimName d) lmodel rmodel))

                        St.modify . second $ M.delete d
                        return b

module Run ( Result (..)
           , SatDict
           , Log
           , runAD
           , runBF
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
import qualified Data.Sequence as SE

import GHC.Generics
import Control.DeepSeq               (NFData)

import Control.Arrow                 (first, second)

import Data.Maybe                    (fromJust, catMaybes)

import Debug.Trace (trace)

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

-- | A zipper for evaluation. This is required so that everytime we have a
-- choice we have access to the rest of the proposition that we are evaluation.
-- If we do not have access then there is no way to perform a selection,
-- manipulate the assertion stack and then continue evaluating.
data Op = CAnd | COr

data Ctx a b = InOpN Op !(SE.Seq (VProp a b), VProp a b)
             | InNot !(Ctx a b)
             | InOpIB Op !(Ctx a b, VProp a b)

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
runEnv f conf !x = _runEnv (f x') conf _emptySt
  where !x' = foldr' ($!) x (opts conf)

runAD :: SMTConf String
      -> VProp String String
      -> IO (V String (Maybe S.SatResult))
runAD os p = (bimap id (fmap S.SatResult)) . unbox . fst' <$>
             runEnv runAndDecomp os p

runBF :: SMTConf String
      -> VProp String String
      -> IO (V String (Maybe S.SatResult))
runBF os p = unRes . fst' <$> runEnv runBruteForce os p
  where unRes (BF x) = x

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
runBruteForce :: (MonadTrans t, Show a, Ord a, MonadState (SatDict a) (t IO)) =>
  VProp a a -> t IO Result
runBruteForce prop = lift $ flip evalStateT (initSt prop) $
  do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> sequence $! (y, selectVariant y prop)) <$> confs
  plainMs <- lift $ mapM (bitraverse pure (S.sat . symbolicPropExpr)) $! catMaybes plainProps
  return . BF . bimap dimName Just .  fromJust $ recompile plainMs

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (MonadTrans t, Monad (t IO)) => VProp String String -> t IO Result
runAndDecomp prop = do
  res <- lift . S.runSMT $ do
    p <- symbolicPropExpr $ andDecomp prop dimName
    SC.query $ do S.constrain p; getVSMTModel
  lift . return . V $ Plain res

runVSMTSolve :: (MonadTrans t, MonadReader (SMTConf String) (t IO)) =>
  VProp String String -> t IO Result
runVSMTSolve prop =
  do cnf <- ask
     res <- lift . S.runSMTWith (conf cnf) . vSMTSolve $
       St.evalStateT (propToSBool prop) (M.empty, M.empty)
     lift . return . V $ res

-- | main workhorse for running the SAT solver
data Result = L (Maybe S.SatResult)
            | BF (V String (Maybe S.SatResult))
            | V (V String (Maybe S.SMTResult))
            deriving (Generic)

-- | unbox a result to get the SMTResults
unbox :: Result -> V String (Maybe S.SMTResult)
unbox (L _)  = Plain Nothing
unbox (BF _) = Plain Nothing -- short circuiting the BF solution
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
type IncState a = (V String (Maybe a), UsedDims Dim)

-- | the incremental solve monad, with the base monad being the query monad so
-- we can pull out sbv models Hardcoding so that I don't have to write the mtl
-- typeclass. I do not expect these to change much
type IncVSMTSolve a = St.StateT (IncState S.SMTResult) SC.Query a

-- | Solve a VSMT proposition
vSMTSolve :: S.Symbolic (VProp S.SBool SNum)
          -> S.Symbolic (V String (Maybe S.SMTResult))
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
  I.SolverContext (StateT (IncState a) m) where
  constrain = lift . S.constrain
  namedConstraint = (lift .) . S.namedConstraint
  setOption = lift . S.setOption


-- notice that we are doing a left fold here
-- vSMTSolveHelper :: S.SBool -> [VProp S.SBool SNum] ->
--   (S.SBool -> S.SBool -> S.SBool) -> IncVSMTSolve S.SBool
-- vSMTSolveHelper !acc ![]     _ = return acc
-- vSMTSolveHelper !acc !(x:xs) f = do b <- vSMTSolve_ x
--                                     let res = b `f` acc
--                                     S.constrain res
--                                     vSMTSolveHelper res xs f

-- | smartly grab a result from the state checking to make sure that if the
-- result is variational than that is preferred over a redundant model. Or in
-- other words, models only occur in leaves of the V tree
getResult :: IncVSMTSolve (V String (Maybe S.SMTResult))
getResult = do (res, _) <- get
               if isEmpty res
                 then lift getVSMTModel >>= return . Plain
                 else return res
  where isEmpty (Plain Nothing) = True
        isEmpty _               = False

clearSt :: IncVSMTSolve ()
clearSt = St.modify . first $ const (Plain Nothing)

store :: V String (Maybe S.SMTResult) -> IncVSMTSolve ()
store = St.modify . first . const

-- | Handle a choice in the IncVSMTSolve monad, we check to make sure that if a
-- choice is already selected then the selection is maintained. If not then we
-- solve both branches by first clearing the state, recursively solving, if the
-- recursive answer is plain then we'll get back a model from the solver, if not
-- then we'll get back a variational model, if we get back a variational model
-- then we reconstruct the choice expression representing the model and store it
-- in the state
handleChc :: Ctx S.SBool SNum -> IncVSMTSolve S.SBool
handleChc (InOpN op (ctx, ChcB d l r)) =
  do (_, used) <- get
     case M.lookup d used of
       Just True  -> handleChc goLeft
       Just False -> handleChc goRight
       Nothing    -> do
                        clearSt
                        St.modify . second $ M.insert d True
                        lift $! SC.push 1
                        handleChc goLeft >>= S.constrain
                        lRes <- getResult
                        lift $! SC.pop 1

                        clearSt
                        St.modify . second $ M.adjust (const False) d
                        lift $! SC.push 1
                        handleChc goRight >>= S.constrain
                        rRes <- getResult
                        lift $! SC.pop 1

                        store $ VChc (dimName d) lRes rRes
                        St.modify . second $ M.delete d
     -- this return statement should never matter because we've reset the
     -- assertion stack. So I just return r' here to fulfill the type
                        return true

  where goLeft  = InOpN op (ctx, l)
        goRight = InOpN op (ctx, r)

handleChc (InOpN _  (SE.Empty, fcs)) = vSMTSolve_ fcs
handleChc (InOpN op (ctx, fcs)) = do
  fcs' <- vSMTSolve_ fcs
  let (newCtx SE.:> newFocus) = SE.viewr ctx
      op' = handler op
  b <- (handleChc $ InOpN op (newCtx, newFocus)) >>= return . op' fcs'
  S.constrain b
  return b
  where handler CAnd = (S.&&&)
        handler COr  = (S.|||)

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl
vSMTSolve_ :: VProp S.SBool SNum -> IncVSMTSolve S.SBool
vSMTSolve_ !(RefB b) = return b
vSMTSolve_ !(LitB b) = return $ S.literal b
vSMTSolve_ !(OpB Not bs)= do b <- vSMTSolve_ bs
                             S.constrain $ S.bnot b
                             return b
vSMTSolve_ !(OpBB op l r) = do bl <- vSMTSolve_ l
                               br <- vSMTSolve_ r
                               let op' = handler op
                               S.constrain $ bl `op'` br
                               return $ bl `op'` br
  where handler Impl   = (==>)
        handler BiImpl = (<=>)
        handler XOr    = (<+>)
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

vSMTSolve_ !(Opn And ps) = handleChc  . InOpN CAnd $ (ctx, fcs)
  where (ctx SE.:> fcs) = SE.viewr ps
vSMTSolve_ !(Opn Or ps) = handleChc  . InOpN COr $ (ctx, fcs)
  where (ctx SE.:> fcs) = SE.viewr ps
vSMTSolve_ x = handleChc (InOpN CAnd (SE.empty, x))

handleSBoolChc :: V Dim S.SBool -> IncVSMTSolve S.SBool
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

reifyArithChcs :: V Dim SNum -> V Dim SNum
  -> (SNum -> SNum -> S.SBool)
  -> IncVSMTSolve (V Dim S.SBool)
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

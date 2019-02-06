module CaseStudy.Auto.Run where

import           Data.SBV
import           Data.SBV.Control
import           Control.Monad (when)
import           Data.SBV.Internals (SolverContext)
import           Data.Bitraversable (bitraverse)
import qualified Data.Sequence  as Seq
import qualified Control.Monad.State.Strict as St

import           CaseStudy.Auto.Lang
import           Run (IncPack, smtBool, smtInt)
import           VProp.Types (Prim, SNum(..), PrimN(..))
import qualified VProp.SBV as SB

import Debug.Trace (trace)

autoToSBool :: (Show a, Ord a) => AutoLang a a -> IncPack a (AutoLang SBool SNum)
autoToSBool = bitraverse smtBool smtInt

instance (Show a, Ord a) => SB.SAT (AutoLang a a) where
  toPredicate p = St.evalStateT (autoToSBool p) (mempty, mempty) >>= evalAutoExpr

-- St.evalStateT (autoToSBool prop) (mempty, mempty)

newtype Queue a = Queue {getQueue :: Seq.Seq a }
  deriving (Show, Functor, Applicative, Monad)
type IncSolve a b = St.StateT (Queue (a, b)) Query

isEmpty :: Queue a -> Bool
isEmpty = Seq.null . getQueue

emptyQ :: Queue a
emptyQ = Queue mempty

drainQ :: Queue a -> Queue a
drainQ = const emptyQ

enq :: a -> Queue a -> Queue a
enq x = Queue . ((Seq.:<|) x) . getQueue

deq :: Show a => Queue a -> (a, Queue a)
deq queue = (el, Queue q)
  where (q Seq.:> el) = Seq.viewr $ getQueue queue

-- | dequeue but don't return the element
popQ :: Show a => Queue a -> Queue a
popQ q
  | isEmpty q = emptyQ
  | otherwise = snd $ deq q

peek :: Show a => Queue a -> a
peek = fst . deq

peekM :: (EqSymbolic a, Show a, Show b) => a -> IncSolve a b SBool
peekM a = do queue <- St.get
             if isEmpty queue
               then return false
               else do let (a', _) = trace ("peekM: not-empty " ++ show queue) $ peek queue
                       return $ a .== a'

getBoolandDeq :: (Show a, Show b) => (St.MonadState (Queue (a, b)) m) => m b
getBoolandDeq = do st <- St.get
                   let ((_, b), newQ) = trace ("deqqing on: " ++ show st) $ deq st
                   St.put newQ
                   return b

instance (SolverContext (IncSolve a b)) where
  constrain = St.lift . constrain
  namedConstraint = (St.lift .) . namedConstraint
  setOption = St.lift . setOption

runIncrementalSolve :: (Show a, Ord a) => [AutoLang a a] -> IO [SatResult]
runIncrementalSolve = mapM (fmap SatResult . runIncrementalSolve_)

runIncrementalSolve_ :: (Show a, Ord a) => AutoLang a a -> IO SMTResult
runIncrementalSolve_ prop = runSMT $
  do prop' <- St.evalStateT (autoToSBool prop) (mempty, mempty)
     query $
       do b <- St.evalStateT (autoSolve prop') emptyQ
          constrain b
          getSMTResult

isLeftMost :: AutoLang a b -> Bool
isLeftMost (BBinary _ l _)
  | isLeaf l = True
  | otherwise = isLeftMost l
  where isLeaf (AutoLit _) = True
        isLeaf (AutoRef _) = True
        isLeaf _           = False
isLeftMost (RBinary _ l _)
  | isLeaf l = True
  | otherwise = isLeftMost' l
  where isLeaf (ALit _) = True
        isLeaf (AVar _) = True
        isLeaf _        = False
isLeftMost (AutoNot (AutoLit _)) = True
isLeftMost (AutoNot (AutoRef _)) = True
isLeftMost (AutoNot e)           = isLeftMost e
isLeftMost _                     = False

isLeftMost' :: ALang a -> Bool
isLeftMost' (ABinary _ l _)
  | isLeaf l = True
  | otherwise = isLeftMost' l
  where isLeaf (ALit _) = True
        isLeaf (AVar _) = True
        isLeaf _        = False
isLeftMost' (Neg (ALit _)) = True
isLeftMost' (Neg (AVar _)) = True
isLeftMost' (Neg e)        = isLeftMost' e
isLeftMost' _              = False

autoSolve :: AutoLang SBool SNum ->
             IncSolve (AutoLang SBool SNum) SBool SBool
autoSolve (AutoLit b) = return $ literal b
autoSolve (AutoRef r) = return r
autoSolve (AutoNot e) = bnot <$> autoSolve e
autoSolve a@(BBinary op l r) = do onQ <- peekM a
                                  i <- St.lift getAssertionStackDepth
                                  St.lift $ io $ putStrLn $ "\n|||||||||" ++ show onQ ++ "||||||"
                                  when (i == 0) $ St.modify drainQ
                                    -- St.lift $ io $ putStrLn "\n-----\n"
                                    -- St.lift $ io $ putStrLn $ "term: " ++ show a
                                  St.lift $ io $ putStrLn $ "AsStack Depth: " ++ show i
                                    -- St.lift $ io $ putStrLn "-----\n"
                                  St.liftM3 iteLazy (return false) -- (return onQ)
                                    (trace ("calling get BOOl" ++ show onQ ++ "\n") $ getBoolandDeq) $
                                    do St.lift $ io $ putStrLn "\nIn ELSE\n"
                                       if (isLeftMost a)
                                         then St.lift resetAssertions
                                         else St.lift $ pop 1
                                       l' <- autoSolve l
                                       r' <- autoSolve r
                                       let op' = bDispatch op
                                           b = l' `op'` r'
                                       St.lift $ push 1
                                       constrain b
                                       St.modify popQ
                                       St.modify $ enq (a,b)
                                       return b
autoSolve a@(RBinary op l r) = do onQ <- peekM a
                                  i <- St.lift getAssertionStackDepth
                                  St.lift $ io $ putStrLn $ "\n|||||||||" ++ show onQ ++ "||||||\n"
                                  when (i == 0) $ St.modify drainQ
                                    -- St.lift $ io $ putStrLn $ "term: " ++ show a
                                    -- St.lift $ io $ putStrLn $ "AsStack Depth: " ++ show i
                                  St.liftM3 iteLazy (return onQ) getBoolandDeq $
                                    do if (isLeftMost a)
                                         then St.lift resetAssertions
                                         else St.lift $ pop 1
                                       l' <- autoSolve' l
                                       r' <- autoSolve' r
                                       let op' = nDispatch op
                                           b   = l' `op'` r'
                                       St.lift $ push 1
                                       constrain b
                                       St.modify popQ
                                       St.modify $ enq (a,b)
                                       return b
autoSolve (Ctx _ _ _)       = error "You probably need to call `idEncode` to convert these ctxes. A Ctx is _only_ used for translating an AutoLang to a VProp "

autoSolve' :: ALang SNum -> IncSolve (AutoLang SBool SNum) SBool SNum
autoSolve' (ALit i) = return $ SI $ literal $ fromIntegral i
autoSolve' (AVar v) = return v
autoSolve' (Neg e) = negate <$> autoSolve' e
autoSolve' (ABinary op l r) = do l' <- autoSolve' l
                                 r' <- autoSolve' r
                                 let op' = aDispatch op
                                     b = l' `op'` r'
                                 return b
autoSolve' (ACtx _ )        = error "You probably need to call `idEncode` to convert these ctxes. A Ctx is _only_ used for translating an AutoLang to a VProp "

bDispatch :: BOp -> SBool -> SBool -> SBool
bDispatch And  = (&&&)
bDispatch  Or  = (|||)
bDispatch Impl = (==>)
bDispatch Eqv  = (<=>)
bDispatch Xor  = (<+>)

nDispatch :: (OrdSymbolic n, Prim SBool n) => RBOp -> n -> n -> SBool
nDispatch GRT  = (.>)
nDispatch GRTE = (.>=)
nDispatch EQL  = (.==)
nDispatch LST  = (.<)
nDispatch LSTE = (.<=)
nDispatch NEQL = (.==)

aDispatch :: (PrimN a) => AOp -> a -> a -> a
aDispatch Add      = (+)
aDispatch Subtract = (-)
aDispatch Multiply = (*)
aDispatch Divide   = (./)
aDispatch Modulus  = (.%)

evalAutoExpr :: AutoLang SBool SNum -> Symbolic SBool
evalAutoExpr (AutoLit b) = return $ literal b
evalAutoExpr (AutoRef r) = return r
evalAutoExpr (AutoNot e) = bnot <$> evalAutoExpr e
evalAutoExpr (BBinary op l r) = do l' <- evalAutoExpr l
                                   r' <- evalAutoExpr r
                                   let op' = bDispatch op
                                       b = l' `op'` r'
                                   constrain b
                                   return b
evalAutoExpr (RBinary op l r) = do l' <- evalAutoExpr' l
                                   r' <- evalAutoExpr' r
                                   let op' = nDispatch op
                                       b   = l' `op'` r'
                                   constrain b
                                   return b
evalAutoExpr _ = error "[ERR] in evalAutoExpr; perhaps you forgot to call idEncode?"

evalAutoExpr' :: ALang SNum -> Symbolic SNum
evalAutoExpr' (ALit i) = return . SI . literal $ fromIntegral i
evalAutoExpr' (AVar a) = return a
evalAutoExpr' (Neg e)  = negate <$> evalAutoExpr' e
evalAutoExpr' (ABinary op l r) = do l' <- evalAutoExpr' l
                                    r' <- evalAutoExpr' r
                                    let op' = aDispatch op
                                        b   = l' `op'` r'
                                    return b
evalAutoExpr' _ = error "[ERR] in evalAutoExpr'; perhaps you forgot to call idEncode?"

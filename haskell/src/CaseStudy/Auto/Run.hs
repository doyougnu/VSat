module CaseStudy.Auto.Run where

import           Data.SBV
import           Data.SBV.Control
import           Data.SBV.Internals (SolverContext)
import           Data.Bitraversable (bitraverse)
import qualified Data.Sequence  as Seq
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as St

import           CaseStudy.Auto.Lang
import           Run (IncPack, smtBool, smtInt)
import           VProp.Types (Prim, SNum(..), PrimN(..))

autoToSBool :: (Show a, Ord a) => AutoLang a a -> IncPack a (AutoLang SBool SNum)
autoToSBool = bitraverse smtBool smtInt

newtype Queue a = Queue {getQueue :: Seq.Seq a }
  deriving (Show, Functor, Applicative, Monad)
type IncSolve a b = L.StateT (Queue (a, b)) Query

isEmpty :: Queue a -> Bool
isEmpty = Seq.null . getQueue

emptyQ :: Queue a
emptyQ = Queue mempty

drainQ :: Queue a -> Queue a
drainQ = const emptyQ

-- emptyAndPop :: (MonadTrans t, L.MonadState (Queue a) m) => t m ()
-- emptyAndPop = do L.modify drainQ
--                  resetAssertions

enq :: a -> Queue a -> Queue a
enq x = Queue . ((Seq.:<|) x) . getQueue

deq :: Queue a -> (a, Queue a)
deq queue = (el, Queue q)
  where (q Seq.:> el) = Seq.viewr $ getQueue queue

peek :: Queue a -> a
peek = fst . deq

peekM :: (EqSymbolic a) => a -> IncSolve a b SBool
peekM a = do queue <- L.get
             if isEmpty queue
               then return false
               else do let (a', _) = peek queue
                       return $ a .== a'

getBoolandDeq :: (L.MonadState (Queue (a, b)) m) => m b
getBoolandDeq = do st <- L.get
                   let ((_, b), newQ) = deq st
                   L.put newQ
                   return b

instance (SolverContext (IncSolve a b)) where
  constrain = L.lift . constrain
  namedConstraint = (L.lift .) . namedConstraint
  setOption = L.lift . setOption

runIncrementalSolve :: (Show a, Ord a) => [AutoLang a a] -> IO [SatResult]
runIncrementalSolve = mapM (fmap SatResult . runIncrementalSolve_)

runIncrementalSolve_ :: (Show a, Ord a) => AutoLang a a -> IO SMTResult
runIncrementalSolve_ prop = runSMT $
  do prop' <- St.evalStateT (autoToSBool prop) (mempty, mempty)
     query $
       do b <- L.evalStateT (autoSolve prop') emptyQ
          constrain b
          getSMTResult

autoSolve :: AutoLang SBool SNum ->
             IncSolve (AutoLang SBool SNum) SBool SBool
autoSolve (AutoLit b) = return $ literal b
autoSolve (AutoRef r) = return r
autoSolve (AutoNot e) = bnot <$> autoSolve e
autoSolve a@(BBinary op l r) = do onQ <- peekM a
                                  i <- L.lift getAssertionStackDepth
                                  if (i == 0)
                                    then  do L.modify drainQ
                                             l' <- autoSolve l
                                             r' <- autoSolve r
                                             let op' = bDispatch op
                                                 b = l' `op'` r'
                                             L.lift $ push 1
                                             constrain b
                                             L.modify $ enq (a,b)
                                             return b
                                    else do
                                    -- L.lift $ io $ putStrLn "\n-----\n"
                                    -- L.lift $ io $ putStrLn $ "term: " ++ show a
                                    -- L.lift $ io $ putStrLn $ "AsStack Depth: " ++ show i
                                    -- L.lift $ io $ putStrLn "-----\n"
                                    L.liftM3 ite (return onQ) getBoolandDeq $
                                      do L.modify drainQ
                                         -- L.lift $ pop 1
                                         L.lift resetAssertions
                                         l' <- autoSolve l
                                         r' <- autoSolve r
                                         let op' = bDispatch op
                                             b = l' `op'` r'
                                         L.lift $ push 1
                                         constrain b
                                         L.modify $ enq (a,b)
                                         return b
autoSolve a@(RBinary op l r) = do onQ <- peekM a
                                  i <- L.lift getAssertionStackDepth
                                  if (i == 0)
                                    then  do L.modify drainQ
                                             l' <- autoSolve' l
                                             r' <- autoSolve' r
                                             let op' = nDispatch op
                                                 b = l' `op'` r'
                                             L.lift $ push 1
                                             constrain b
                                             L.modify $ enq (a,b)
                                             return b
                                    else do
                                    -- L.lift $ io $ putStrLn $ "term: " ++ show a
                                    -- L.lift $ io $ putStrLn $ "AsStack Depth: " ++ show i
                                    L.liftM3 ite (return onQ) getBoolandDeq $
                                      do L.modify drainQ
                                         -- L.lift resetAssertions
                                         L.lift $ pop 1
                                         l' <- autoSolve' l
                                         r' <- autoSolve' r
                                         let op' = nDispatch op
                                             b   = l' `op'` r'
                                         L.lift $ push 1
                                         constrain b
                                         L.modify $ enq (a,b)
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

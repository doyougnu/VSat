module CaseStudy.Auto.Run where

import           Data.SBV
import           Data.SBV.Control
import           Data.SBV.Internals (SolverContext)
import           Data.Bitraversable (bitraverse)
import qualified Control.Monad.State.Lazy as St

import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Lang
import           Run (IncPack, smtBool, smtInt)
import           VProp.Types (Prim, SNum(..), NPrim(..), PrimN(..))

autoToSBool :: (Show a, Ord a) => AutoLang a a -> IncPack a (AutoLang SBool SNum)
autoToSBool = bitraverse smtBool smtInt

data Queue a = Queue [a] [a]
type IncSolve a b = St.StateT (Queue (a, b)) Query

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

emptyQ :: Queue a -> Queue a
emptyQ q
  | isEmpty q = q
  | otherwise = emptyQ . snd . deq $ q

-- emptyAndPop :: (MonadTrans t, St.MonadState (Queue a) m) => t m ()
-- emptyAndPop = do St.modify emptyQ
--                  resetAssertions

enq :: a -> Queue a -> Queue a
enq x (Queue xs ys) = Queue xs (x:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue [] [])     = error "empty queue"
deq (Queue [] ys)     = deq $ Queue (reverse ys) []
deq (Queue (x:xs) ys) = (x, Queue xs ys)

peek :: Queue a -> a
peek = fst . deq

peekM :: Eq a => a -> IncSolve a b Bool
peekM a = do queue <- St.get
             let (a', _) = peek queue
             return $ a == a'

getBoolandDeq :: (St.MonadState (Queue (a, b)) m) => m b
getBoolandDeq = do st <- St.get
                   let ((_, b), newQ) = deq st
                   St.put newQ
                   return b
  -- St.get >>= return . snd . fst . deq

instance (SolverContext (IncSolve a b)) where
  constrain = St.lift . constrain
  namedConstraint = (St.lift .) . namedConstraint
  setOption = St.lift . setOption

autoSolve :: AutoLang SBool SNum ->
             IncSolve (AutoLang SBool SNum) SBool SBool
autoSolve (AutoLit b) = return $ literal b
autoSolve (AutoRef r) = return r
autoSolve (AutoNot e) = bnot <$> autoSolve e
autoSolve a@(BBinary op l r) = do onQ <- peekM a
                                  if onQ
                                    then getBoolandDeq
                                    else do St.modify emptyQ
                                            St.lift resetAssertions
                                            l' <- autoSolve l
                                            r' <- autoSolve r
                                            let op' = bDispatch op
                                                b = l' `op'` r'
                                            St.lift $ push 1
                                            constrain b
                                            St.modify $ enq (a,b)
                                            return b
autoSolve a@(RBinary op l r) = do onQ <- peekM a
                                  if onQ
                                    then getBoolandDeq
                                    else do St.modify emptyQ
                                            St.lift resetAssertions
                                            l' <- autoSolve' l
                                            r' <- autoSolve' r
                                            let op' = nDispatch op
                                                b   = l' `op'` r'
                                            St.lift $ push 1
                                            constrain b
                                            St.modify $ enq (a,b)
                                            return b

autoSolve' :: ALang SNum -> IncSolve (AutoLang SBool SNum) SBool SNum
autoSolve' (ALit i) = return $ SI $ literal $ fromIntegral i
autoSolve' (AVar v) = return v
autoSolve' (Neg e) = negate <$> autoSolve' e
autoSolve' (ABinary op l r) = do l' <- autoSolve' l
                                 r' <- autoSolve' r
                                 let op' = aDispatch op
                                     b = l' `op'` r'
                                 return b

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

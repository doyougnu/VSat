module CaseStudy.Auto.Run where

import           Data.SBV
import           Data.SBV.Control
import           Data.SBV.Internals (SolverContext)
import qualified Control.Monad.State.Lazy as St

import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Lang
import           Run (IncPack, smtBool)
import           VProp.Types (Prim)


-- propToSBool :: (Show a,Ord a) => VProp d a a -> IncPack a (VProp d S.SBool SNum)
-- propToSBool !(RefB x)     = RefB   <$> smtBool x
-- propToSBool !(OpB o e)    = OpB  o <$> propToSBool e
-- propToSBool !(OpBB o l r) = OpBB o <$> propToSBool l <*> propToSBool r
-- propToSBool !(ChcB d l r) = ChcB d <$> propToSBool l <*> propToSBool r
-- propToSBool !(OpIB o l r) = OpIB o <$> propToSBool' l <*> propToSBool' r
-- propToSBool !(LitB b)     = return $ LitB b

-- propToSBool' :: (Ord b, Show b) => VIExpr d b -> IncPack b (VIExpr d SNum)
-- propToSBool' !(Ref RefI i) = Ref RefI <$> smtInt i
-- propToSBool' !(Ref RefD d) = Ref RefD <$> smtDouble d
-- propToSBool' !(OpI o e)    = OpI o    <$> propToSBool' e
-- propToSBool' !(OpII o l r) = OpII o <$> propToSBool' l <*> propToSBool' r
-- propToSBool' !(ChcI d l r) = ChcI d <$> propToSBool' l <*> propToSBool' r
-- propToSBool' !(LitI x)     = return $ LitI x

autoToSBool :: (Show a, Ord a) => AutoLang a -> IncPack a (AutoLang SBool)
autoToSBool = mapM smtBool

data Queue a = Queue [a] [a]
type IncSolve a = St.StateT (Queue (a, SBool)) Query

enq :: a -> Queue a -> Queue a
enq x (Queue xs ys) = Queue xs (x:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue [] [])     = error "empty queue"
deq (Queue [] ys)     = deq $ Queue (reverse ys) []
deq (Queue (x:xs) ys) = (x, Queue xs ys)

peek :: Queue a -> a
peek = fst . deq

peekM :: Eq a => a -> IncSolve a Bool
peekM a = do queue <- St.get
             let (a', b) = peek queue
             return $ a == a'

getQueueBool :: IncSolve a SBool
getQueueBool = St.get >>= return . snd . fst . deq

instance (Monad m, SolverContext m) =>
  SolverContext (St.StateT (IncSolve a b) m) where
  constrain = St.lift . constrain
  namedConstraint = (St.lift .) . namedConstraint
  setOption = St.lift . setOption

autoSolve :: AutoLang SBool -> IncSolve (AutoLang SBool) SBool
autoSolve a@(AutoLit b) = return $ literal b
autoSolve a@(AutoNot e) = bnot <$> autoSolve e
autoSolve a@(BBinary op l r) = do onQ <- peekM a
                                  if onQ
                                    then getQueueBool
                                    else do l' <- autoSolve l
                                            r' <- autoSolve r
                                            let op' = bDispatch op
                                                b = l' `op'` r'
                                            St.lift $ push 1
                                            constrain b
                                            St.modify $ enq (a,b)
                                            return b
autoSolve a@(RBinary op l r) = do onQ <- peekM a
                                  if onQ
                                    then getQueueBool
                                    else do l' <- autoSolve' l
                                            r' <- autoSolve' r
                                            let op' = nDispatch op
                                                b = l' `op'` r'
                                            St.lift $ push 1
                                            constrain b
                                            St.modify $ enq (a,b)
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

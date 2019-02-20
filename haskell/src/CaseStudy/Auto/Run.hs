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

-- | a stack to follow what terms are on the assertion stack
newtype Stack a = Stack {getStack :: Seq.Seq a }
  deriving (Show, Functor, Applicative, Monad)

-- | simple monad stack holding a stack of pairs, a is the term we Eq on, b is
-- the SBool that results from teh evaluation of a
type IncSolve a b = St.StateT (Stack (a, b)) Query

isEmpty :: Stack a -> Bool
isEmpty = Seq.null . getStack

isEmptyM :: (St.MonadState (Stack a) m) => m Bool
isEmptyM = St.gets isEmpty

emptyS :: Stack a
emptyS = Stack mempty

drainS :: Stack a -> Stack a
drainS = const emptyS

pushS :: a -> Stack a -> Stack a
pushS x = Stack . (Seq.<|) x . getStack

pop' :: Stack a -> (a, Stack a)
pop' stack = (el, Stack q)
  where (el Seq.:< q) = Seq.viewl $ getStack stack

-- | dequeue but don't return the element
popS :: Stack a -> Stack a
popS q
  | isEmpty q = emptyS
  | otherwise = snd $ pop' q

popM :: (St.MonadState (Stack a) (t Query), St.MonadTrans t) => t Query ()
popM = do St.modify' popS
          i <- St.lift $ getAssertionStackDepth
          if (i > 0)
            then St.lift $ pop 1
            else return ()

pushM :: (St.MonadState (Stack a) (t Query), St.MonadTrans t) => a -> t Query ()
pushM a = do St.modify' (pushS a); St.lift $ push 1

-- | given a stack get the first
peek :: Stack a -> a
peek = fst <$> pop'

-- | given a stack get the first
peekBool :: Stack (a, b) -> b
peekBool = (snd . fst) <$> pop'

-- | check that a given a, is the top of the stack without manipulating the
-- stack
peekM :: (EqSymbolic a, St.MonadState (Stack (a, b)) m) => a -> m SBool
peekM a = do stack <- St.get
             if isEmpty stack
               then return false
               else do let (a', _) = peek stack
                       return $ a .== a'

-- | pop the top element off the stack
getBool :: (St.MonadState (Stack (a, b)) m) => m b
getBool = St.gets helper
  where
    helper stk
          | isEmpty stk = error "[ERR]: Calling getBool on empty stack"
          | otherwise = peekBool stk

instance (SolverContext (IncSolve a b)) where
  constrain = St.lift . constrain
  namedConstraint = (St.lift .) . namedConstraint
  setOption = St.lift . setOption

runIncrementalSolve :: (Show a, Ord a) => [AutoLang a a] -> IO [SatResult]
runIncrementalSolve xs = fmap (fmap SatResult) $ runIncrementalSolve_ $ trace (show x' ++ "\n") x'
  where x' = take 2 xs

runIncrementalSolve_ :: (Show a, Ord a) => [AutoLang a a] -> IO [SMTResult]
runIncrementalSolve_ props = runSMT $
  do props' <- St.evalStateT (mapM autoToSBool props) (mempty, mempty)
     query $
       do bs <- St.evalStateT (mapM autoSolve props') emptyS
          mapM (\b -> do constrain b; getSMTResult) bs

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

-- | whileM_ version for SBools
-- whileM_ :: (Monad m, Mergeable (m ())) => m SBool -> m a -> m ()
instance Mergeable (Query ()) where
  symbolicMerge _ _ _ _ = return ()
  select _ _ _          = return ()

instance Mergeable (IncSolve a b ()) where
  symbolicMerge _ _ _ _ = return ()
  select _ _ _          = return ()

-- whileM_ :: Query SBool -> Query a -> Query ()
whileM_ :: (Show a, Show b) => IncSolve a b SBool -> IncSolve a b SBool -> IncSolve a b c -> IncSolve a b ()
whileM_ p q f = go
  where go = do x <- p
                y <- q
                s <- St.get
                m <- isEmptyM
                trace (show s ++ "  :  " ++ (show m)) $ return ()
                -- the problem is in peekM
                -- iteLazy (x ||| y) (f >> go) (return ())
                iteLazy (x) (f >> go) (return ())
                -- iteLazy (false) (f >> go) (return ())

autoSolve :: AutoLang SBool SNum ->
             IncSolve (AutoLang SBool SNum) SBool SBool
autoSolve (AutoLit b) = return $ literal b
autoSolve a@(AutoRef r) = do St.modify' (pushS (a, r))
                             St.lift $ push 1
                             return r
autoSolve (AutoNot e) = bnot <$> autoSolve e
autoSolve a@(BBinary op l r) =
  do q <- St.get
     trace ("INBB: stack: " ++ show q) $ return ()
     St.lift $ io $ putStrLn "\n-----\n"
     St.lift $ io $ putStrLn $ "term: " ++ show a
     St.lift $ io $ putStrLn "-----\n"
     -- we peek at the top, if we have a match we return it
     St.liftM3 iteLazy (peekM a) getBool $
       -- if it is not on the top level then we pop until we match or until the
       -- stack is empty
       do St.lift $ io $ putStrLn "\nIn ELSE\n"
          let notOnStk = bnot <$> peekM a
              stkNotEmpty = ((bnot . literal) <$> isEmptyM)
          whileM_ notOnStk stkNotEmpty popM
          St.liftM3 iteLazy (peekM a) getBool $
            do
              l' <- autoSolve l
              r' <- autoSolve r
              let op' = bDispatch op
                  b = l' `op'` r'
              St.lift $ push 1
              constrain b
              St.modify $ pushS (a,b)
              return b
autoSolve a@(RBinary op l r) = do onQ <- peekM a
                                  i <- St.lift getAssertionStackDepth
                                  St.lift $ io $ putStrLn $ "\n|||||||||" ++ show onQ ++ "||||||\n"
                                  when (i == 0) $ St.modify drainS
                                  St.lift $ io $ putStrLn $ "term: " ++ show a
                                  St.lift $ io $ putStrLn $ "AsStack Depth: " ++ show i
                                  St.liftM3 iteLazy (return onQ) getBool $
                                    do l' <- autoSolve' l
                                       r' <- autoSolve' r
                                       let op' = nDispatch op
                                           b   = l' `op'` r'
                                       St.lift $ push 1
                                       constrain b
                                       St.modify popS
                                       St.modify $ pushS (a,b)
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

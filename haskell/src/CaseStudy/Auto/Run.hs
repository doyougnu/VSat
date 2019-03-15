module CaseStudy.Auto.Run where

import           Data.SBV
import           Data.SBV.Control
import           Control.Arrow (first)
import           Control.Monad (foldM)
import           Data.Text (Text)
import qualified Data.List as L (sort, groupBy, lookup,filter)
import           Data.Map
import           Data.SBV.Internals (SolverContext)
import           Data.Bitraversable (bitraverse)
import qualified Data.Sequence  as Seq
import qualified Control.Monad.State.Strict as St

import           CaseStudy.Auto.Lang
import           Run (IncPack, smtBool, smtInt)
import           VProp.Types (Prim, SNum(..), PrimN(..),VProp(..))
import qualified VProp.SBV as SB


import SAT
import Result

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

-- | check if the current context is sat or not
isSat :: Query Bool
isSat = do cs <- checkSat
           return $ case cs of
                      Sat -> True
                      _      -> False

instance (SolverContext (IncSolve a b)) where
  constrain = St.lift . constrain
  namedConstraint = (St.lift .) . namedConstraint
  setOption = St.lift . setOption

runIncrementalSolve :: [AutoLang Text Text] -> IO (Result Text)
runIncrementalSolve xs = runIncrementalSolve_ (toList <$> assocMaps)
  where assocList = L.groupBy isPlain $ L.sort $ fmap (first helper) splitCtx <$> xs
        assocMaps' = unions $ (fromListWith (flip $ BBinary And) <$> assocList)
        assocMaps = St.evalStateT (mapM (autoToSBool) assocMaps') (mempty,mempty)
        helper x | hasCtx x = x
                 | otherwise = AutoRef "__plain__"


runIncrementalSolve_ :: Symbolic [(AutoLang Text Text, AutoLang SBool SNum)] ->
  IO (Result Text)
runIncrementalSolve_  assocList = runSMT $
  do assocList' <- assocList
     let (Just ps) = L.lookup (AutoRef "__plain__") assocList'
         rest = L.filter (not . (==(AutoRef "__plain__")) . fst) assocList'
         dispatchProp :: ResultProp d -> Bool -> ResultProp d
         dispatchProp !p !x = if x
                              then p
                              else negateResultProp p
     ps' <- evalAutoExpr ps
     query $
       do
         push 1
         constrain ps'
         b <- isSat
         resMap <- if b
                   then
                     do let plainProp = autoToResProp (AutoRef "__plain__")
                        pm <- getResult (dispatchProp plainProp)
                        if not $ isResultNull pm
                          then return $! insertToSat plainProp pm
                          else return mempty
                   else return mempty

         foldM (\acc (v, prop) ->
              do
               io $ putStrLn $ show v
               runIncrementalSolve__ prop
               a <- isSat
               resMap' <- if a
                          then
                            do let prop = autoToResProp v
                               pm <- getResult (dispatchProp prop)
                               if not $ isResultNull pm
                                 then return $! insertToSat prop pm
                                 else return mempty
                          else return mempty
               return $! acc <> resMap'
           ) resMap rest


runIncrementalSolve__ :: AutoLang SBool SNum -> Query ()
runIncrementalSolve__ prop =
       do
         bs <- St.evalStateT (autoSolve prop) emptyS
         constrain bs

-- autoToResProp :: AutoLang a a -> ResultProp a
autoToResProp :: AutoLang Text Text -> ResultProp Text
autoToResProp = ResultProp . Just . UniformProp . helper
  where helper (RBinary _ (ACtx (AVar a)) _) = RefB a
        helper (AutoRef a) = RefB a

splitCtx :: AutoLang a b -> (AutoLang a b, AutoLang a b)
splitCtx a = (leftMost a, removeCtxs a)

hasCtx :: AutoLang a b -> Bool
hasCtx (Ctx _ _ _) = True
hasCtx (AutoNot e) = hasCtx e
hasCtx (BBinary _ l r) = hasCtx l || hasCtx r
hasCtx (RBinary _ l r) = hasCtx' l || hasCtx' r
hasCtx _               = False

hasCtx' :: ALang a -> Bool
hasCtx' (ACtx _) = True
hasCtx' (Neg e)  = hasCtx' e
hasCtx' (ABinary _ l r) = hasCtx' l && hasCtx' r
hasCtx' _               = False

isPlain :: (Eq a, Eq b) =>
  (AutoLang a b, AutoLang a b) -> (AutoLang a b, AutoLang a b) -> Bool
isPlain (x, _) (y, _) = x == y

removeCtxs :: AutoLang a b -> AutoLang a b
removeCtxs (Ctx _ _ rest) = rest
removeCtxs (BBinary op l r)
  | hasCtx l = r
  | otherwise = (BBinary op (removeCtxs l) (removeCtxs r))
removeCtxs a@(RBinary _ _ _) = a
  -- (RBinary op (removeCtxs' l) (removeCtxs' r))
removeCtxs (AutoNot e) = AutoNot $ removeCtxs e
removeCtxs x = x


leftMost :: AutoLang a b -> (AutoLang a b)
leftMost (BBinary Impl l _)
  | hasCtxForm l = l
  | otherwise = leftMost l
  where hasCtxForm (RBinary _ (ACtx _) _) = True
        hasCtxForm (BBinary _ l' r)        = hasCtxForm l' && hasCtxForm r
        hasCtxForm _                      = False
leftMost (BBinary _ l _)
  | isLeaf l = l
  | otherwise = leftMost l
  where isLeaf (AutoLit _) = True
        isLeaf (AutoRef _) = True
        isLeaf _           = False
leftMost (Ctx _ _ a)
  | isLeaf a = a
  | otherwise = leftMost a
  where isLeaf (AutoLit _) = True
        isLeaf (AutoRef _) = True
        isLeaf _           = False
leftMost (AutoNot e)       = leftMost e
leftMost x@(RBinary _ (ACtx _) (ALit _)) = x
leftMost x@(RBinary _ _ _)   = x
leftMost x                 = x

leftMost' :: ALang a -> ALang a
leftMost' (ABinary _ l _)
  | isLeaf l = l
  | otherwise = leftMost' l
  where isLeaf (ALit _) = True
        isLeaf (AVar _) = True
        isLeaf _        = False
leftMost' (Neg e)       = leftMost' e
leftMost' x             = x

-- | whileM_ version for SBools
-- whileM_ :: (Monad m, Mergeable (m ())) => m SBool -> m a -> m ()
instance Mergeable (Query ()) where
  symbolicMerge _ _ _ _ = return ()
  select _ _ _          = return ()

instance Mergeable (IncSolve a b ()) where
  symbolicMerge _ _ _ _ = return ()
  select _ _ _          = return ()

whileM_ :: (Show a, Show b) =>
  IncSolve a b SBool -> IncSolve a b c -> IncSolve a b ()
whileM_ p f = go
  where go = do x <- p; iteLazy x (f >> go) (return ())

autoSolve :: AutoLang SBool SNum ->
             IncSolve (AutoLang SBool SNum) SBool SBool
autoSolve (AutoLit b) = return $ literal b
autoSolve a@(AutoRef r) = do St.modify' (pushS (a, r))
                             St.lift $ push 1
                             return r
autoSolve (AutoNot e) = bnot <$> autoSolve e
autoSolve a@(BBinary op l r) =
  do
     -- trace ("INBB: stack: " ++ show q) $ return ()
     -- St.lift $ io $ putStrLn "\n-----\n"
     -- St.lift $ io $ putStrLn $ "term: " ++ show a
     -- St.lift $ io $ putStrLn "-----\n"
     -- we peek at the top, if we have a match we return it
     St.liftM3 iteLazy (peekM a) getBool $
       -- if it is not on the top level then we pop until we match or until the
       -- stack is empty
       do -- St.lift $ io $ putStrLn "\nIn ELSE\n"
          let notOnStk = peekM a
          whileM_ notOnStk popM
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
autoSolve a@(RBinary op l r) = do -- onQ <- peekM a
                                  -- i <- St.lift getAssertionStackDepth
                                  -- St.lift $ io $ putStrLn $ "\n|||||||||" ++ show onQ ++ "||||||\n"
                                  -- when (i == 0) $ St.modify drainS
                                  -- St.lift $ io $ putStrLn $ "term: " ++ show a
                                  -- St.lift $ io $ putStrLn $ "AsStack Depth: " ++ show i
                                  St.liftM3 iteLazy (peekM a) getBool $
                                    do
                                      let notOnStk = peekM a
                                      whileM_ notOnStk popM
                                      St.liftM3 iteLazy (peekM a) getBool $
                                        do
                                          l' <- autoSolve' l
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

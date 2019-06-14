module CaseStudy.Auto.Run where

import           Data.SBV
import           Data.SBV.Control
import           Control.Arrow (first)
import           Control.Monad (foldM)
import           Data.Text (Text,pack)
import qualified Data.List as L (partition, sort, groupBy, lookup,filter)
import           Data.Map
import           Data.SBV.Internals (SolverContext)
import           Data.Bitraversable (bitraverse)
import qualified Data.Sequence  as Seq
import qualified Control.Monad.State.Strict as St

import           CaseStudy.Auto.Lang
import           CaseStudy.Auto.Auto
import           Run (IncPack, smtBoolWith, smtInt)
import           VProp.Types (Prim, SNum(..), PrimN(..),VProp(..),VIExpr(..),RefN(..),NPrim(..))
import qualified VProp.SBV as SB


import SAT
import Result

import Debug.Trace (trace)

autoToSBool :: (Show a, Ord a) => AutoLang a a -> IncPack a (AutoLang SBool SNum)
autoToSBool = bitraverse (flip smtBoolWith show) smtInt

instance (Show a, Ord a) => SB.SAT (AutoLang a a) where
  toPredicate p = St.evalStateT (autoToSBool p) (mempty, mempty) >>= evalAutoExpr

-- | a stack to follow what terms are on the assertion stack
newtype Stack a = Stack { getStack :: Seq.Seq a }
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
                      _   -> False

instance (SolverContext (IncSolve a b)) where
  constrain = St.lift . constrain
  namedConstraint = (St.lift .) . namedConstraint
  setOption = St.lift . setOption

runIncrementalSolve :: [AutoLang Text Text] -> IO (Result Text)
runIncrementalSolve xs = runIncrementalSolve_ (toList <$> assocMaps)
  where assocList = makeAssocList xs
        assocMaps' = unions $ (fromListWith (flip $ BBinary And) <$> assocList)
        assocMaps = trace (show (keys assocMaps')) $
                    (fmap evalAutoExpr__) <$>
                    St.evalStateT (mapM (autoToSBool) assocMaps') (mempty,mempty)

-- | Make an association list. this finds a context by position, if it exists it
-- is captured in the lhs of a tuple, if not then we create a ref called
-- "__plain__", then we sort and group on these split terms. This results in a
-- list of lists which contain all the associated terms for a single evolution
-- context, so the list for say evo_ctx < 1 will have _every_ formula that
-- pretained to that context
makeAssocList :: [AutoLang Text Text] -> [[(AutoLang Text Text, AutoLang Text Text)]]
makeAssocList xs = L.groupBy isPlain $
                   L.sort $
                   fmap (first helper) splitCtx <$> xs
  where helper x
          | hasCtx x = x
          | otherwise = AutoRef "__plain__"

-- | a map of contexts to formulas, we abuse the types here
type AssocMap = Map (AutoLang Text Text) (AutoLang Text Text)

-- | This is a helper function for the AssocMap type. It expects to only work on
-- the keys of the map and is only counting the number of ctxs in those keys
countCtxs :: AutoLang Text Text -> Int
countCtxs (RBinary _ (ACtx _) _) = 1
countCtxs (AutoNot e) = countCtxs e
countCtxs (BBinary _ l r) = countCtxs l + countCtxs r
countCtxs _ = 0


-- | this is particular to the data, we check each evolution context and
-- convolve the ones that overlap i.e., evo_ctx <= 2, must consider the <1, <=1
-- <=0 and <0 case, this is no handled through renaming the contexts
correctFormulas :: AssocMap -> AssocMap
correctFormulas m = expandedMap
  where
    ctxs = mapKeys getEvoCtx m
    m' = mapWithKey (\k v -> (autoAndJoin' $ v : concatMap (flip findLessThan  ctxs) k)) ctxs
    expandedMap = mapKeys helper m'
      where helper []  = AutoRef "__plain__"
            helper xs = autoAndJoin' $ reconstruct <$> xs
            reconstruct (op, i) = (RBinary op (ACtx (AVar (pack "evo_ctx"))) i)


getEvoCtx :: AutoLang Text Text -> [EvoContext Text]
getEvoCtx (BBinary _
           (RBinary op _ i)
           (RBinary op' _ i')) = [(op, i), (op',i')]
getEvoCtx (RBinary op _ i) = pure (op, i)
getEvoCtx _ = []

-- | Given an evo context, find all of the formulas that correspond to evo
-- contexts that are less than the input one
findLessThan :: EvoContext Text -> Map [EvoContext Text] (AutoLang Text Text) -> [AutoLang Text Text]
findLessThan e = elems . filterWithKey (\ks _ -> any (< e) ks)


runIncrementalSolve_ :: Symbolic [(AutoLang Text Text, Query SBool)] ->
  IO (Result Text)
runIncrementalSolve_  assocList = runSMT $
  do assocList' <- assocList
     let (Just ps) = L.lookup (AutoRef "__plain__") assocList'
         (singletons, rest) = L.partition ((<2) . countCtxs . fst) assocList'

     query $
       do
         push 1
         ps >>= constrain
         b <- isSat
         resMap <- if b
                   then
                     do let plainProp = autoToResProp (AutoRef "__plain__")
                        pm <- getResult plainProp
                        if not $ isResultNull pm
                          then return $! insertToSat plainProp pm
                          else return mempty
                   else return mempty

         -- run the points in time that need to not accumulate on the assertion
         -- stack
         foldM (\acc (v, prop) ->
              do
               trace ("solving: " ++ show v ++ "\n") $ return ()
               push 1
               prop >>= constrain
               a <- isSat
               trace ("isSat?: " ++ show a ++ "\n") $ return ()
               resMap' <- if a
                          then
                            do let prp = autoToResProp v
                               pm <- getResult prp
                               if not $ isResultNull pm
                                 then return $! insertToSat prp pm
                                 else return mempty
                          else return mempty
               -- we pop here so that the assertion stack will only hold the
               -- plains
               pop 1
               return $! acc <> resMap'
           ) resMap rest

         foldM (\acc (v, prop) ->
              do
               trace ("solving: " ++ show v ++ "\n") $ return ()
               push 1
               prop >>= constrain
               a <- isSat
               trace ("isSat?: " ++ show a ++ "\n") $ return ()
               resMap' <- if a
                          then
                            do let prp = autoToResProp v
                               pm <- getResult prp
                               if not $ isResultNull pm
                                 then return $! insertToSat prp pm
                                 else return mempty
                          else return mempty
               -- don't pop here so that we continue to accumulate on the
               -- assertion stack
               -- pop 1
               return $! acc <> resMap'
           ) resMap singletons


runIncrementalSolve__ :: AutoLang SBool SNum -> Query ()
runIncrementalSolve__ prop = evalAutoExpr__ prop >>= constrain

-- autoToResProp :: AutoLang a a -> ResultProp a
autoToResProp :: AutoLang Text Text -> ResultProp Text
autoToResProp = ResultProp . Just . UniformProp . helper
  where helper (RBinary op (ACtx (AVar a)) (ALit i))
          = OpIB (idispatch op) (Ref RefI a) (LitI $ I (fromInteger i))
        helper (BBinary op l r) = OpBB (rdispatch' op) (helper l) (helper r)
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
              -- constrain b
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
                                          -- constrain b
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

evalAutoExpr__ :: AutoLang SBool SNum -> Query SBool
evalAutoExpr__ (AutoLit b) = return $ literal b
evalAutoExpr__ (AutoRef r) = return r
evalAutoExpr__ (AutoNot e) = bnot <$> evalAutoExpr__ e
evalAutoExpr__ (BBinary op l r) = do l' <- evalAutoExpr__ l
                                     r' <- evalAutoExpr__ r
                                     let op' = bDispatch op
                                         b = l' `op'` r'
                                     constrain b
                                     return b
evalAutoExpr__ (RBinary op l r) = do l' <- evalAutoExpr__' l
                                     r' <- evalAutoExpr__' r
                                     let op' = nDispatch op
                                         b   = l' `op'` r'
                                     constrain b
                                     return b
evalAutoExpr__ _ = error "[ERR] in evalAutoExpr__; perhaps you forgot to call idEncode?"

evalAutoExpr__' :: ALang SNum -> Query SNum
evalAutoExpr__' (ALit i) = return . SI . literal $ fromIntegral i
evalAutoExpr__' (AVar a) = return a
evalAutoExpr__' (Neg e)  = negate <$> evalAutoExpr__' e
evalAutoExpr__' (ABinary op l r) = do l' <- evalAutoExpr__' l
                                      r' <- evalAutoExpr__' r
                                      let op' = aDispatch op
                                          b   = l' `op'` r'
                                      return b
evalAutoExpr__' _ = error "[ERR] in evalAutoExpr__'; perhaps you forgot to call idEncode?"

module Run ( runEnv
           , Opts (..)
           ) where

import qualified Data.Map.Strict as M
import Control.Monad.RWS.Strict
import qualified Data.SBV.Internals  as I
import qualified Data.SBV            as S
import qualified Data.SBV.Control    as SC

import qualified Data.Set            as Set
import Data.Foldable                 (foldr')
import Data.List                     (partition, (\\), nub, lookup)
import Data.Char                     (isUpper)
import Control.Arrow                 ((***))

import Data.Maybe                    (fromJust, fromMaybe)

import VProp
import V

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- and values are whether that config is satisfiable or not (a bool)
type SatDict = (M.Map Config Bool, M.Map Var Bool) -- keys may incur perf penalty

-- | The optimizations that could be set
data Opts = Opts { runBaselines :: Bool              -- ^ Run baselines?
                 , runAD :: Bool                     -- ^ run anddecomp baseline? else Brute Force
                 , runOpts :: Bool                   -- ^ Run optimizations or not?
                 , optimizations :: [VProp -> VProp] -- ^ a list of optimizations
                 }

-- | Type convenience for Log
type Log = String

-- | Takes a dimension d, a value a, and a result r
type Env r = RWST Opts Log SatDict IO r -- ^ the monad stack

-- | An empty reader monad environment, in the future read these from config file
_emptyOpts :: Opts
_emptyOpts = Opts { runBaselines = False
                  , runAD = False
                  , runOpts = False
                  , optimizations = []
                  }


_setOpts :: Bool -> Bool -> Bool -> [VProp -> VProp] -> Opts
_setOpts base bAD bOpt opts = Opts { runBaselines = base
                                  , runAD = bAD
                                  , runOpts = bOpt
                                  , optimizations = opts
                                  }



-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env r -> Opts -> SatDict -> IO (r, SatDict,  Log)
_runEnv m opts st = runRWST m opts st

-- TODO use configurate and load the config from a file
runEnv :: Bool -> Bool -> Bool -> [VProp -> VProp] -> VProp -> IO (Result, SatDict, Log)
runEnv base bAD bOpt opts x = _runEnv
                             (work x)
                             (_setOpts base bAD bOpt opts)
                             (initSt x)


-- | Given a VProp term generate the satisfiability map
initSt :: VProp -> SatDict
initSt prop = (sats, vs)
  where sats = M.fromList . fmap (\x -> (x, False)) . Set.toList $ paths prop
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
runBruteForce :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO [S.SatResult]
runBruteForce prop = do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> (y, selectVariant y prop)) <$> confs
  plainModels <- lift $ mapM (S.sat . symbolicPropExpr . fromJust . snd) plainProps
  return plainModels

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: VProp -> IO (Maybe I.SMTModel)
runAndDecomp prop = S.runSMT $ do
  p <- symbolicPropExpr $ (andDecomp prop)
  S.constrain p
  SC.query $ do
    c <- SC.checkSat
    case c of
      SC.Unk -> error "asdf"
      SC.Unsat -> return Nothing
      SC.Sat -> do model' <- SC.getModel
                   return $ Just model'

-- | main workhorse for running the SAT solver
data Result = R (Maybe I.SMTModel)
            | L [S.SatResult]
            | Vr (V Dim (Maybe I.SMTModel))

work :: ( MonadTrans t
        , MonadState SatDict (t IO)
        , MonadReader Opts (t IO)) => VProp -> t IO Result
work prop = do
  baselines <- asks runBaselines
  bAD <- asks runAD
  -- fix this antipattern later
  if baselines
    then if bAD
         then lift $ runAndDecomp prop >>= return . R
         else runBruteForce prop >>= return . L
    else do
    opts <- asks optimizations
    result <- lift $ incrementalSolve prop opts
    return $ Vr result

-- | given VProp, incrementally solve it using variational tricks and SBV
incrementalSolve :: VProp -> [VProp -> VProp] -> IO (V Dim (Maybe I.SMTModel))
incrementalSolve prop opts = solveChoice prop Nothing


solveChoice :: VProp -> Maybe I.SMTModel -> IO (V Dim (Maybe I.SMTModel))
solveChoice prop model
  | isPlain prop = S.runSMT $ do
      p <- symbolicPropExpr prop
      S.constrain p
      SC.query $ do c <- SC.checkSat
                    case c of
                      SC.Unk   -> error "SBV failed in plain solve Choice"
                      SC.Unsat -> return $ Plain Nothing
                      SC.Sat   -> SC.getModel >>= return . Plain . Just
  | otherwise = S.runSMT $ do
      let ps = fmap M.toList . Set.toList $ paths prop
          dims = Set.toList $ dimensions prop
          fauxModel = I.SMTModel{ I.modelAssocs = zipWith (\x y -> (dimName x, y))
                                                  dims (repeat I.falseCW)
                                , I.modelObjectives=[]
                                }
          newModel = combineModels model (Just fauxModel)

          dimBoolMap :: [Dim] -> S.Symbolic [(Dim, S.SBool)]
          dimBoolMap =  traverse (\x -> sequence (x, S.sBool $ dimName x))

          mkPaths :: [(Dim, S.SBool)] -> [[(Dim, Bool)]] -> [[(Dim, S.SBool, Bool)]]
          mkPaths dimBools pths =
            fmap (\(dim, bl) -> (dim, fromJust $ lookup dim dimBools, bl)) <$> pths

          cConstrain :: (Dim, S.SBool, Bool) -> SC.Query ()
          cConstrain (_, dim, b) = assocToConstraint (dim, b) >>= S.constrain

          cQuery :: [(Dim, S.SBool, Bool)] -> SC.Query (Maybe I.SMTModel)
          cQuery x = do SC.push 1
                        mapM_ cConstrain x
                        c <- SC.checkSat
                        case c of
                          SC.Unk   -> error "asdf"
                          SC.Unsat -> return Nothing
                          SC.Sat   -> do model' <- SC.getModel
                                         SC.pop 1
                                         return $ Just model'

      ds <- dimBoolMap dims
      p <- symbolicPropExpr' prop newModel ds
      S.constrain p
      let madePaths = mkPaths ds ps
      res <- SC.query $ do
        mapM (\x -> sequence
               ( M.fromList $ fmap (\(dim, _, bl) -> (dim, bl)) x
               , cQuery x)) madePaths
      return . fromJust $ V.recompile res

combineModels :: Maybe I.SMTModel -> Maybe I.SMTModel -> Maybe I.SMTModel
combineModels Nothing a = a
combineModels a Nothing = a
combineModels
  (Just I.SMTModel{I.modelAssocs=aAs, I.modelObjectives=aOs})
  (Just I.SMTModel{I.modelAssocs=bAs , I.modelObjectives=bOs}) =
  (Just I.SMTModel{ I.modelAssocs= nub aAs ++ bAs
                  , I.modelObjectives = aOs ++ bOs})


updateProp :: VProp -> Maybe I.SMTModel -> VProp
updateProp prop Nothing = prop
updateProp prop (Just model) = selectedDims
  where
    assignments = I.modelAssocs model
    (dims, vs) = partition (all isUpper . fst) assignments
    replacedRefs = foldr' (\(var, val) accProp -> refToLit (Var var) (const $ I.cwToBool val) accProp) prop vs
    selectedDims = toCNF $
                   pruneTagTree
                   (M.fromList ((Dim *** I.cwToBool) <$> dims)) replacedRefs

assocToConstraint :: (S.SBool, Bool) -> SC.Query S.SBool
assocToConstraint (var, val) = return $ var S..== (bToSb val)
  where bToSb True = S.true
        bToSb False = S.false

dimModels :: VProp -> [I.SMTModel]
dimModels prop = mkModel <$> models''
  where dims = Set.toList $ dimensions prop
        models' = [(dimName d, b) | d <- dims, b <- [I.trueCW, I.falseCW]]
        models'' = do
          a <- models'
          b <- models'
          guard $ (fst a) /= (fst b)
          return [a, b]
        models = take (length models'' `div` 2) models''
        mkModel m = I.SMTModel{I.modelObjectives=[], I.modelAssocs=m}

isModelNull :: I.SMTModel -> Bool
isModelNull I.SMTModel{I.modelAssocs=as, I.modelObjectives=os} = null as && null os

isOnlyLits :: VProp -> Bool
isOnlyLits (Lit _) = S.true
isOnlyLits (Ref _) = S.false
isOnlyLits (Chc _ _ _) = S.false
isOnlyLits (VProp.Not p) = isOnlyLits p
isOnlyLits (Opn _ ps) = all isOnlyLits ps
isOnlyLits (Op2 _ l r) = isOnlyLits l && isOnlyLits r

select :: VProp -> Maybe VProp
select (Opn _ ps) = safeHead [ p | p <- ps ]
  where safeHead [] = Nothing
        safeHead (x:_) = Just x
select (Op2 _ l _) = Just l
select (VProp.Not x) = Just x
select (Chc _ l _) = Just l
select x         = Just x

-- | Change a prop to a predicate, avoiding anything that has already been assigned
symbolicPropExpr' :: VProp -> Maybe I.SMTModel -> [(Dim, S.SBool)] -> S.Predicate
symbolicPropExpr' prop Nothing _ = symbolicPropExpr prop
symbolicPropExpr' prop (Just model) as = do
    let vs = (Set.toList (vars prop)) \\ (Var <$> assignedVs)
        ds = (Set.toList (dimensions prop)) \\ (Dim <$> assignedDs)
        assignments = fst <$> I.modelAssocs model
        (assignedDs, assignedVs) = partition (all isUpper) assignments
    syms <- fmap (M.fromList . zip vs) (S.sBools (map varName vs))
    dims <- fmap (M.fromList . zip ds) (S.sBools (map dimName ds))
    let look f = fromMaybe err (M.lookup f syms)
        lookd d = fromMaybe errd (M.lookup d (dims `M.union` (M.fromList as)))

    return (evalPropExpr lookd look prop)

  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."

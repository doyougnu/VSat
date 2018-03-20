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
import Data.List                     (partition, (\\))
import Data.Char                     (isUpper)
import Control.Arrow                 ((***))

import Data.Maybe                    (fromJust, isJust, fromMaybe)

import VProp

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
runBruteForce :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO VProp
runBruteForce prop = do
  (_confs, _) <- get
  let confs = M.keys _confs
      plainProps = (\y -> (y, selectVariant y prop)) <$> confs
  mapM_ work' plainProps
  (newSats, _) <- get
  return $ recompile prop $ (\(x, y) -> (x, show y)) <$> M.toList newSats

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

-- | If then else, the way it should've been defined in Prelude
if' :: Bool -> a -> a -> a
if' True a _  = a
if' False _ b = b

-- | main workhorse for running the SAT solver
data Result = R (Maybe I.SMTModel)
            | L [Maybe I.SMTModel]
            | V VProp

work :: ( MonadTrans t
        , MonadState SatDict (t IO)
        , MonadReader Opts (t IO)) => VProp -> t IO Result
work prop = do
  baselines <- asks runBaselines
  bAD <- asks runAD
  -- fix this antipattern later
  if baselines
    then if bAD
         then do r <- lift $ runAndDecomp prop
                 return $ R r
         else do r <- runBruteForce prop
                 return $ V r
    else do
    opts <- asks optimizations
    result <- lift $ incrementalSolve prop opts
    return $ R result

-- | return all possible models incrementally
incrementalSolveAll :: VProp -> [VProp -> VProp] -> IO [Maybe I.SMTModel]
incrementalSolveAll prop opts = do
  let props = grabProps $ toCNF prop
      models_ = dimModels prop
  mapM (selectAndSolve props . Just) models_
    where grabProps (Opn VProp.And props) = props
          grabProps x                     = pure x

-- | given VProp, incrementally solve it using variational tricks and SBV
incrementalSolve :: VProp -> [VProp -> VProp] -> IO (Maybe I.SMTModel)
incrementalSolve prop opts = do
  let props = grabProps $ toCNF prop
  selectAndSolve props Nothing
    where grabProps (Opn VProp.And props) = props
          grabProps x                     = pure x


-- | Solve a vprop expression by choosing a subterm, solving it, updating the
-- state and repeating
selectAndSolve :: [VProp] -> Maybe I.SMTModel -> IO (Maybe I.SMTModel)
selectAndSolve [] model = return model
selectAndSolve (prop:ps) model = do
  newModel <- S.runSMT $ incrementalQuery prop model
  -- let ps' = (flip updateProp newModel <$> ps) -- TODO don't rewrite the props with their values, this is combinatoric, we want SBV to handle the naming and constraint propogation
  selectAndSolve ps newModel

incrementalQuery :: VProp -> Maybe I.SMTModel -> I.Symbolic (Maybe I.SMTModel)
incrementalQuery prop model
  | isOnlyLits prop = return model
  | otherwise = do
  -- assumptions <- modelToConstraint model
  p <- symbolicPropExpr' prop model
  SC.query $ do
    S.constrain p
    -- c <- SC.checkSatAssuming assumptions
    c <- SC.checkSat
    case c of
      SC.Unk -> error "asdf"
      SC.Unsat -> return Nothing
      SC.Sat -> do model' <- SC.getModel
                   return $ Just model'

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

assocToConstraint :: (String, I.CW) -> S.Symbolic S.SBool
assocToConstraint (var, val) = do v <- S.sBool var
                                  return $ v S..== (bToSb boolVal)
  where boolVal = I.cwToBool val
        bToSb True = S.true
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


modelToConstraint :: Maybe I.SMTModel -> I.Symbolic [S.SBool]
modelToConstraint Nothing = return []
modelToConstraint (Just model)
  | isModelNull model = return []
  | otherwise = mapM assocToConstraint (I.modelAssocs model)

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

work' :: ( MonadTrans t
         , MonadState SatDict (t IO)) => (Config, Maybe VProp) -> t IO ()
work' (conf, plainProp) = when (isJust plainProp) $
  do (sats, vs) <- get
     result <- lift . S.isSatisfiable . symbolicPropExpr . fromJust $ plainProp
     put (M.insert conf result sats, vs)

getIsoDims :: VProp -> VProp
getIsoDims (Opn a ps) = Opn a $ getIsoDims <$> filter isChc ps
getIsoDims (Op2 a l r) = (getIsoDims l) S.&&& (getIsoDims r)
getIsoDims (Not ps)    = getIsoDims ps
getIsoDims (Chc d l r) = Chc d (getIsoDims l) (getIsoDims r)
getIsoDims (Lit a)     = Lit False
getIsoDims (Ref a)     = Ref . Var $ ""

-- | Change a prop to a predicate, avoiding anything that has already been assigned
symbolicPropExpr' :: VProp -> Maybe I.SMTModel -> S.Predicate
symbolicPropExpr' prop Nothing = symbolicPropExpr prop
symbolicPropExpr' prop (Just model) = do
    let vs = (Set.toList (vars prop)) \\ (Var <$> assignedVs)
        ds = (Set.toList (dimensions prop)) \\ (Dim <$> assignedDs)
        assignments = fst <$> I.modelAssocs model
        (assignedVs, assignedDs) = partition (all isUpper) assignments
    syms <- fmap (M.fromList . zip vs) (S.sBools (map varName vs))
    dims <- fmap (M.fromList . zip ds) (S.sBools (map dimName ds))
    let look f = fromMaybe err (M.lookup f syms)
        lookd d = fromMaybe errd (M.lookup d dims)
    return (evalPropExpr lookd look prop)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."

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
import Data.List                     (partition)
import Data.Char                     (isUpper)
import Control.Arrow                 ((***))

import Data.SBV                      (isSatisfiable)
import Data.Maybe                    (fromJust, isJust, fromMaybe)

import Debug.Trace                   (trace)

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
runEnv :: Bool -> Bool -> Bool -> [VProp -> VProp] -> VProp -> IO (VProp, SatDict, Log)
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
  return . recompile prop $ (\(x, y) -> (x, show y)) <$> M.toList newSats

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: (MonadTrans t, MonadState SatDict (t IO)) => VProp -> t IO VProp
runAndDecomp prop = do
  (sats, _) <- get
  result <- lift . isSatisfiable . symbolicPropExpr $ (andDecomp prop)
  return . recompile prop . fmap (\(x, y) -> (x, show y)) . M.toList $ M.map (const result) sats

-- | If then else, the way it should've been defined in Prelude
if' :: Bool -> a -> a -> a
if' True a _  = a
if' False _ b = b

-- | pick a term to evaluate
-- select :: VProp -> VProp
-- select (Opn And (x:_)) = x

-- | main workhorse for running the SAT solver
work :: ( MonadTrans t
        , MonadState SatDict (t IO)
        , MonadReader Opts (t IO)) => VProp -> t IO VProp
work prop = do
  baselines <- asks runBaselines
  bAD <- asks runAD
  -- fix this antipattern later
  if baselines
    then if' bAD (runAndDecomp prop) (runBruteForce prop)
    else if' bAD (runAndDecomp prop) (runBruteForce prop)

-- | given VProp, incrementally solve it using variational tricks and SBV
incrementalSolve :: VProp -> [VProp -> VProp] -> I.Symbolic (Maybe I.SMTModel)
incrementalSolve prop opts = do
  let props = grabProps $ toCNF prop
  selectAndSolve props Nothing
    where grabProps (Opn VProp.And props) = props
          grabProps x                     = pure x

-- | Solve a vprop expression by choosing a subterm, solving it, updating the
-- state and repeating
selectAndSolve :: [VProp] -> Maybe I.SMTModel -> I.Symbolic (Maybe I.SMTModel)
selectAndSolve [] model = return model
selectAndSolve (prop:ps) model = do
  newModel <- incrementalQuery prop model
  let ps' = (flip updateProp newModel <$> ps)
  selectAndSolve ps' newModel

-- incrementalQuery :: VProp -> Maybe I.SMTModel -> I.Symbolic (Maybe I.SMTModel)
incrementalQuery :: VProp -> Maybe I.SMTModel -> I.Symbolic (Maybe I.SMTModel)
incrementalQuery prop model
  | isOnlyLits prop = return model
  | otherwise = do
  p <- symbolicPropExpr prop
  trace ("\n The prop \n" ++ show prop ++ "\n The model: \n" ++ show model) $ return ()
  I.constrain p
  assumptions <- modelToConstraint model
  SC.query $ do
    c <- SC.checkSatAssuming assumptions
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

-- assocToConstraint :: (String, I.CW) -> I.Symbolic ()
-- assocToConstraint (var, val) = do v <- S.sBool var
--                                   I.constrain $ v S..== (bToSb boolVal)
--                                     where boolVal = I.cwToBool val
--                                           bToSb True = S.true
--                                           bToSb False = S.false

assocToConstraint :: (String, I.CW) -> S.Symbolic S.SBool
assocToConstraint (var, val) = do v <- S.sBool var
                                  return $ v S..== (bToSb boolVal)
  where boolVal = I.cwToBool val
        bToSb True = S.true
        bToSb False = S.false

modelToConstraint :: Maybe I.SMTModel -> I.Symbolic [S.SBool]
modelToConstraint Nothing = return []
modelToConstraint (Just model)
  | isModelNull model = return []
  | otherwise = do
      trace ("\n adding constraints \n" ++ show model) $ return ()
      mapM assocToConstraint (I.modelAssocs model)

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
     result <- lift . isSatisfiable . symbolicPropExpr . fromJust $ plainProp
     put (M.insert conf result sats, vs)

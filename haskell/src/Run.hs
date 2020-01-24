{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Run ( SatDict
           , Log
           , runAD
           , runBF
           , runVSMT
           , fst'
           , IncPack
           , smtBoolWith
           , smtInt
           , BValue(..)
           , evaluate
           , toBValue
           , propToSBool
           , emptySt
           , runPonV
           , runVonP
           , vCoreMetrics
           ) where

import           Control.Arrow (first, second)
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict as St
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           Control.DeepSeq
import           GHC.Generics
import           GHC.Conc (ThreadId, threadDelay)
import qualified Data.Set as Set
import           Data.Foldable (foldr')
import           Data.HashSet (HashSet, member, insert)
import qualified Data.Map.Strict as M
import qualified Data.SBV as S
import qualified Data.SBV.Control as SC
import qualified Data.SBV.Internals as I
import qualified Data.SBV.Trans as Ts
import qualified Data.SBV.Trans.Control as Tsc
import           Data.Text (unpack, pack, Text)
import qualified Data.Text.IO as T
import           Data.List (intersperse)
import           Prelude hiding (LT, GT, EQ)
import           Control.Concurrent (forkIO, forkOS)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.Async
import           System.IO

import           SAT
import           VProp.Types
import           VProp.SBV
import           VProp.Core
import           Utils
import           Config
import           Result

-- import           Text.Show.Unicode          (ushow)
import Debug.Trace

-- | The satisfiable dictionary, this is actually the "state" keys are configs
-- (an mapping from dimensions to booleans denoting selection) and values are
-- whether that config is satisfiable or not (a bool)
type SatDict a = M.Map (Config a) Bool

-- | Type convenience for Log
type Log = Text

-- | Takes a dimension d, a type for values a, and a result r
type Env d = RWST (SMTConf d Text Text) Log (SatDict d) IO

-- | An empty reader monad environment, in the future read these from config
-- file
_emptySt :: SatDict d
_emptySt = M.empty

-- | Run the RWS monad with defaults of empty state, reader
_runEnv :: Env d r ->
           SMTConf d Text Text ->
           SatDict d ->
           IO (r, SatDict d,  Log)
_runEnv = runRWST


runEnv :: (ReadableProp d -> Env d (Result d)) ->
          SMTConf d Text Text ->
          ReadableProp d ->
          IO (Result d, SatDict d, Log)
runEnv f conf x = _runEnv (f x') conf _emptySt
  where x' = foldr' ($!) x (opts conf)

runAD :: (Show d, Resultable d, SAT (ReadableProp d)) =>
         SMTConf d Text Text
      -> ReadableProp d
      -> (d -> Text)
      -> IO (Result d)
runAD os p f = fst' <$> runEnv (flip runAndDecomp f) os p

runBF :: (Resultable d, SAT (ReadableProp d)) =>
         ConfigPool d        ->
         SMTConf d Text Text ->
         ReadableProp d      ->
         IO (Result d)
runBF pool os p = fst' <$> runEnv (runBruteForce pool) os p


runVonP :: (Resultable d, SAT (ReadableProp d)) =>
           ConfigPool d        ->
           SMTConf d Text Text ->
           ReadableProp d      ->
           IO (Result d)
runVonP pool os p = fst' <$> runEnv (runVOnPlain pool) os p

-- runPonV :: (Show d, Resultable d) => ConfigPool d -> ReadableProp d
--         -> IO (Result d, SatDict d, Log)
runPonV pool conf prop = runEnv (runPlainOnVSat pool) conf prop

-- | Run the VSMT solver given a list of optimizations and a prop
runVSMT :: ConfigPool Text        ->
           SMTConf Text Text Text ->
           ReadableProp Text      ->
           IO (Result Text, SatDict Text, Log)
runVSMT = runEnv . runVSMTSolve

-- | Given a VProp a term generate the satisfiability map
initSt :: Ord d => ConfigPool d -> VProp d a a -> SatDict d
initSt []   prop = M.fromList . fmap (\x -> (x, False)) $ choices prop
initSt pool _    = M.fromList $ zip pool (repeat False)

-- | Some logging functions
_logBaseline :: (Show a, MonadWriter [Char] m) => a -> m ()
_logBaseline x = tell $ "Running baseline: " ++ show x

_logCNF :: (Show a, MonadWriter String m) => a -> m ()
_logCNF x = tell $ "Generated CNF: " ++ show x

_logResult :: (Show a, MonadWriter String m) => a -> m ()
_logResult x = tell $ "Got result: " ++ show x

-- | run the sat solver, when we get a satisafiable call get the assignments
-- directly
runForDict :: ( Resultable d
              , MonadIO m
              , MonadReader (SMTConf d a a) m
              , SAT (ReadableProp d)) =>
  (Config d, ReadableProp d) -> m (Result d)
runForDict (configToResultProp -> c, x) = do
  cnf <- ask
  liftIO $ S.runSMTWith (conf cnf) $
    do
      x' <- toPredicate x
      SC.query $
        do S.constrain x'
           res <- getResultWith (\a -> if a then c else negateResultProp c)
           SC.exit
           return res

-- | run the sat solver, when we get a satisafiable call get the assignments
-- directly
runForDict' :: ( Resultable d
              , MonadIO m
              , MonadReader (SMTConf d a a) m
              , SAT (ReadableProp d)) =>
  (Config d, ReadableProp d) -> m (Result d)
runForDict' (configToResultProp -> c, x) = do
  cnf <- ask
  liftIO $ S.runSMTWith (conf cnf) $
    do
      x' <- toPredicate x
      SC.query . SC.inNewAssertionStack $
        do S.constrain x'
           getResultWith (\a -> if a then c else negateResultProp c)

-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runBruteForce ::
  ( MonadIO m
  , Resultable d
  , MonadState (SatDict d) m
  , MonadReader (SMTConf d a a) m
  , SAT (ReadableProp d)) =>
  ConfigPool d        ->
  ReadableProp d      ->
  m (Result d)
runBruteForce pool prop = flip evalStateT (initSt pool prop) $
  do
  _confs <- get
  -- dbg "PROP" prop
  let confs = M.keys _confs
      plainProps = if null confs
        then pure (M.empty, prop)
        else (\y -> (y, selectVariantTotal y prop)) <$> confs
  plainMs <- lift $ mapM runForDict plainProps
  return $! mconcat plainMs

-- | Run the brute force baseline case, that is select every plain variant and
-- run them to the sat solver
runVOnPlain ::
  ( MonadIO m
  , Resultable d
  , MonadState (SatDict d) m
  , MonadReader (SMTConf d a a) m
  , SAT (ReadableProp d)) =>
  ConfigPool d        ->
  ReadableProp d      ->
  m (Result d)
runVOnPlain pool prop = flip evalStateT (initSt pool prop) $
  do
  _confs <- get
  -- dbg "PROP" prop
  let confs = M.keys _confs
      plainProps = if null confs
        then pure (M.empty, prop)
        else (\y -> (y, selectVariantTotal y prop)) <$> confs
  plainMs <- lift $ mapM runForDict' plainProps
  return $! mconcat plainMs

-- | Run plain terms on vsat, that is, perform selection for each dimension, and
-- then run on the vsat solver. We know that this will always become a Unit, and
-- will always hit solvePlain.
runPlainOnVSat
  :: ConfigPool Text -> ReadableProp Text -> Env Text (Result Text)
runPlainOnVSat pool prop = flip evalStateT (initSt pool prop) $
 do
  _confs <- get
  let confs = M.keys _confs
      plainProps = if null confs
        then [(M.empty, prop)]
        else (\y -> (y, selectVariant y prop)) <$> confs
  plainMs <- lift $ mapM (bitraverse
                   (pure . configToResultProp)
                   (runVSMTSolve mempty)) $ plainProps
  return $ foldMap (uncurry helper) plainMs
  where
        helper c as = insertToSat c as

-- | Run the and decomposition baseline case, that is deconstruct every choice
-- and then run the sat solver
runAndDecomp :: ( Resultable d
                , MonadIO m
                , Monad m
                , MonadReader (SMTConf d a a) m
                , SAT (ReadableProp d)) =>
  ReadableProp d -> (d -> Text) -> m (Result d)
runAndDecomp prop f = runForDict (mempty, andDecomp prop (f . dimName))

runVSMTSolve ::
  ConfigPool Text ->
  ReadableProp Text ->
  Env Text (Result Text)
runVSMTSolve !configPool !prop =
  do cnf <- ask
     let
       mkVCore = St.runStateT (propToSBool prop) emptyPackState
       vCore :: S.Symbolic (VProp Text (Ts.SBool, Name) SNum)
       vCore = fmap fst mkVCore
       -- we get a symbolic count back so we just run a quick query to retrieve
       -- the number
       -- TODO push this into the parser
       dCount = S.runSMT $! fmap (Set.size . foundDimensions . snd) mkVCore

     -- run the inner driver
     let
       pprop :: S.Symbolic (Loc Text)
       -- !pprop = findPrincipleChoice . mkTop <$> (vCore >>= (evaluate . toBValue))
       !pprop = mkTop <$> (vCore >>= (evaluate . toBValue))
       run = \configuration -> vSMTSolve pprop configuration cnf dCount

     -- run configurations in parallel after making the variational core. Note
     -- that right now we do not support mixing partial configs with total
     -- configs but this would be a useful feature in the future. This means
     -- that if there is a config pool each config is the same size and hence we
     -- can fmap over them to condition the vCore appropriately, if there are
     -- choices left over than the inner solver will spin up threads to solve
     -- them, if not then we'll just get a model immediately
     results <- liftIO $ mapM run $ if null configPool
                                    then pure mempty
                                    else configPool

     return $! mconcat results

-- | wrapper around map to keep track of the variable references we've seen, a,
-- and their symbolic type, b, which is eta reduced here
type UsedVars a = M.Map a
type NumDimensions = Int
-- | a set of dimensions that have been processed and have generated models
type GenModel = Bool

-- | A state monad transformer that holds two usedvar maps, one for booleans and
-- one for doubles
data PackState a = PackState { usedBools  :: UsedVars a S.SBool
                             , usedNums   :: UsedVars a SNum
                             , foundDimensions :: Set.Set (Dim a)
                             }

emptyPackState = PackState {usedBools=mempty
                           ,usedNums=mempty
                           ,foundDimensions=mempty
                           }

onUsedNums :: (UsedVars a SNum -> UsedVars a SNum) -> PackState a -> PackState a
onUsedNums f PackState{..} = PackState{usedNums=f usedNums, ..}

onUsedBools :: (UsedVars a S.SBool -> UsedVars a S.SBool) ->
               PackState a -> PackState a
onUsedBools f PackState{..} = PackState{usedBools=f usedBools, ..}

addDimension :: Ord a => Dim a -> PackState a -> PackState a
addDimension e PackState{..} =
  PackState{foundDimensions=Set.insert e foundDimensions, ..}


type IncPack a = StateT (PackState a) S.Symbolic

-- | a pool of configurations used to perform selection. A list is appropriate
-- here because we only use it as a stack, or a single traversal and we require
-- the semantics of a possible infinite (co-inductive for you nerds)
-- data structure
type ConfigPool a = [Config a]

type Used = HashSet Text

type UsedConstraint = Text

-- | the internal state for the incremental solve algorithm, it holds a result
-- list, and the used dims map, and is parameterized by the types of dimensions,
-- d
data IncState =
  IncState { config      :: !(Config Text) -- * the current config
           , genModelMaps :: !Bool       -- This is added boilerplate resulting
                                        -- from not have a proper newtype for
                                        -- IncStateVSMT. For VSAT1 this is fine
                                        -- and will be refactored for VSAT2.
           , workerChan :: RequestChan
           , resultChan :: ResultChan
           } deriving (Eq,Generic)

-- emptySt :: IncState
emptySt chan resChan = IncState{ config=mempty
                               , genModelMaps=True
                               , workerChan = chan
                               , resultChan = resChan
                               }

-- onResult :: (Result Text -> Result Text) -> IncState -> IncState
-- onResult f IncState {..} = IncState {result=f result, ..}

onConfig :: (Config Text -> Config Text) -> IncState -> IncState
onConfig f IncState {..} = IncState {config=f config, ..}
{-# INLINE onConfig #-}

deleteFromConfig :: (Dim Text) -> IncState -> IncState
deleteFromConfig = onConfig . M.delete
{-# INLINE deleteFromConfig #-}

insertToConfig :: (Dim Text) -> Bool -> IncState -> IncState
insertToConfig d b = onConfig $ M.insert d b
{-# INLINE insertToConfig #-}

replaceConfig :: Config Text -> IncState -> IncState
replaceConfig = onConfig . const
{-# INLINE replaceConfig #-}

onGenModels :: (Bool -> Bool) -> IncState -> IncState
onGenModels f IncState {..} = IncState {genModelMaps =f genModelMaps, ..}
{-# INLINE onGenModels #-}

-- | the incremental solve monad, with the base monad being the query monad so
-- we can pull out sbv models Hardcoding so that I don't have to write the mtl
-- typeclass. I do not expect these to change much
-- TODO newtype this and add a reader for various settings
type IncVSMTSolver s = ReaderT s Tsc.Query

type IncVSMTSolve = IncVSMTSolver IncState

type RequestChan = InChan (IncState, IncVSMTSolve ())
type ResultChan = InChan (Result Text)

-- | Top level wrapper around engine and monad stack, this sets options for the
-- underlying solver, inspects the results to see if they were variational or
-- not, if not then it gets the plain model
vSMTSolve :: S.Symbolic (Loc Text) ->
             Config Text ->
             SMTConf Text Text Text ->
             IO NumDimensions ->
             IO (Result Text)
vSMTSolve prop propConf cnf i =
  do
    (reqChanIn, reqChanOut) <- newChan
    (resChanIn, resChanOut) <- newChan

    dimensionCount <- i

    -- convenience for spawning a curried worker
    let go = worker prop reqChanOut

        -- kick off a main thread
        runMain = forkIO $ do
          S.runSMTWith (conf cnf) $
            do prop' <- prop
               SC.query $
                 runReaderT
                 (doChoice prop')
                 -- set model generator
                 (onGenModels (const $! generateModels (settings cnf))
                 -- set prop formula configuration of dimensions
                 . (onConfig $ const propConf) $!
                 -- construct an empty state
                  (emptySt reqChanIn resChanIn))

        -- continuation to run in worker thread, takes a state and query computation

        propConfSize = length propConf
        possibleMax = (2^dimensionCount) - if propConfSize == 0
                                           then propConfSize
                                           else 2^propConfSize
        maxResults = if dimensionCount == 0 || possibleMax == 0
                     then 1 else possibleMax
        numWorkers =  if dimensionCount == 0 || possibleMax == 0
                      then 1
                      else possibleMax

    -- dbg "Dim Count" dimensionCount
    -- dbg "Possible Max" possibleMax
    -- dbg "Workers" numWorkers
    -- dbg "Results" maxResults

    -- kick off main thread
    runMain

    -- set up worker threads
    mapM_ go [1..numWorkers]

    -- listen for results on main
    rs <- mapConcurrently (\_ -> readChan resChanOut) [1..maxResults]

    -- accumulate the results
    return $! mconcat rs

solvePlain :: (Resultable d) => SC.Query (Result d)
solvePlain = getResultWith $ toResultProp . LitB

worker :: S.Symbolic (Loc Text) -> OutChan (IncState, IncVSMTSolve ()) -> Int -> IO ThreadId
worker prop requestChan i =
  -- use forkIO here, for some reason mapConcurrently_ errors out
  forkIO $ forever $ do
  -- trace (show i ++ ": " ++ "Waiting for Conf") $ return ()
  (!st, !qry) <- liftIO $ readChan requestChan
  -- trace (show i ++ ": " ++ "Running with CONF" ++ show (config st))  $ return ()
  S.runSMT $!
    do prop' <- prop
       SC.query $! runReaderT qry st

-- | The name of a reference
type Name = Text

-- | The name of a constraint is a list of reference names and operators
type ConstraintName = [Name]

-- | This ensures two things: 1st we need all variables to be symbolic before
-- starting query mode. 2nd we cannot allow any duplicates to be called on a
-- string -> symbolic a function or missiles will launch.
propToSBool :: ReadableProp Text -> IncPack Text (VProp Text (S.SBool, Name) SNum)
propToSBool (RefB x)     = do b <- smtBool x
                              return $! RefB (b, x)
propToSBool (OpB o e)    = OpB  o <$> propToSBool e
propToSBool (OpBB o l r) = OpBB o <$> propToSBool l <*> propToSBool r
propToSBool (ChcB d l r) = do St.modify' $ addDimension d
                              ChcB d <$> propToSBool l <*> propToSBool r
propToSBool (OpIB o l r) = OpIB o <$> propToSBool' l <*> propToSBool' r
propToSBool (LitB b)     = return $ LitB b

propToSBool' :: VIExpr Text Text -> IncPack Text (VIExpr Text SNum)
propToSBool' (Ref RefI i) = Ref RefI <$> smtInt i
propToSBool' (Ref RefD d) = Ref RefD <$> smtDouble d
propToSBool' (OpI o e)    = OpI o    <$> propToSBool' e
propToSBool' (OpII o l r) = OpII o <$> propToSBool' l <*> propToSBool' r
propToSBool' (ChcI d l r) = do St.modify' $ addDimension d
                               ChcI d <$> propToSBool' l <*> propToSBool' r
propToSBool' (LitI x)     = return $ LitI x

-- | a builder function that abstracts out the packing algorithm for numbers. It
-- takes a function to convert a string to a symbolic variable like S.sInt64, or
-- S.sDouble, a constructor that add the symbolic type to the sum type SNum and
-- produces a function k -> t m SNum, which reifies to k -> IncPack k SNum.
mkSmt :: (String -> IncPack Text a) ->
         (a -> SNum) -> Text -> IncPack Text SNum
mkSmt f g str = do st <- gets usedNums
                   case str `M.lookup` st of
                     Nothing -> do b <- f $ show str
                                   let b' = g b
                                   St.modify' (onUsedNums $ M.insert str b')
                                   return b'
                     Just x  -> return x


-- | convert every reference to a boolean, keeping track of what you've seen
-- before, can't use mkStatement here because its a special case
smtBoolWith :: Text -> (Text -> String) -> IncPack Text S.SBool
smtBoolWith str f =
  do st <- gets usedBools
     case str `M.lookup` st of
       Nothing -> do b <- Ts.sBool $ f str
                     St.modify' (onUsedBools $ M.insert str b)
                     return b
       Just x  -> return x

smtBool :: Text -> IncPack Text S.SBool
smtBool = flip smtBoolWith unpack

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtInt :: Text -> IncPack Text SNum
smtInt = mkSmt Ts.sInt64 SI

-- | convert every reference to a Integer, keeping track of what you've seen
-- before
smtDouble :: Text -> IncPack Text SNum
smtDouble = mkSmt Ts.sDouble SD

-- | type class needed to avoid lifting for constraints in the IncSolve monad

instance I.SolverContext (StateT s Tsc.Query) where
  constrain = lift . Ts.constrain
  namedConstraint = (lift .) . Ts.namedConstraint
  setOption = lift . Ts.setOption
  softConstrain = lift . I.softConstrain
  constrainWithAttribute = (lift .) . I.constrainWithAttribute
  contextState = lift I.contextState

instance I.SolverContext (ReaderT s Tsc.Query) where
  constrain = lift . Ts.constrain
  namedConstraint = (lift .) . Ts.namedConstraint
  setOption = lift . Ts.setOption
  softConstrain = lift . I.softConstrain
  constrainWithAttribute = (lift .) . I.constrainWithAttribute
  contextState = lift I.contextState


setDim :: Dim Text -> Bool -> IncVSMTSolve () -> IncVSMTSolve ()
setDim = (local .) . insertToConfig

removeDim :: Dim Text -> IncVSMTSolve () -> IncVSMTSolve ()
removeDim = local . deleteFromConfig

setConfig :: Config Text -> IncVSMTSolve () -> IncVSMTSolve ()
setConfig = local . replaceConfig

-- | A smart constrain method, this inspects a set of constraint names to see if
-- we have a duplicate constraint name. If we do (if the name representing the
-- sbool is in the set) then we simply constrain the bool unnamed, if not then
-- we add the named constraint and insert into the set. We use an unordered
-- hashset for performance reasons because these strings can get quite long
-- leading to poor Eq performance
-- constrain :: S.SBool -> ConstraintName -> IncVSMTSolve ()
-- constrain b [] = Ts.constrain b
-- constrain b !name = do
--   used <- gets usedConstraints
--   if not (isUsed usedName used)
--     then do S.namedConstraint name' b; setUsed usedName
--     else Ts.constrain b
--   where !name' = (unpack $ mconcat (intersperse " " name))
--         !usedName = mconcat name

toText :: Show a => a -> Text
toText = pack . show


solveVariant :: IncVSMTSolve () -> IncVSMTSolve ()
solveVariant go = do
           -- trace "Going!!!" $ return ()
           go

           -- convert configuration to a result propositional formula
           !prop <- asks (configToResultProp . config)
           !modelsEnabled <- asks genModelMaps

           -- grab the model if there is one
           resMap <- if modelsEnabled then getResult prop else getResultOnlySat prop

           -- return the result
           resChan <- asks resultChan
           liftIO $! writeChan resChan resMap


handleChc :: IncVSMTSolve ()
          -> IncVSMTSolve ()
          -> Dim Text
          -> IncVSMTSolve ()
handleChc goLeft goRight d =
  -- trace "handle chc" $
  do currentCfg <- asks config
     case M.lookup d currentCfg of
       Just True  -> goLeft
       Just False -> goRight
       Nothing    -> do
         chan' <- asks workerChan
         st <- ask
         liftIO $! writeChan chan' (insertToConfig d False st, goRight)

         -- liftIO $! writeChan chan' (insertToConfig d True st, goLeft)
         setDim d True goLeft
{-# INLINE handleChc #-}

-- | type synonym to simplify the BValue type
type SBVProp d = VProp d (S.SBool, Name) SNum

-- | A BValue is a type to represent the AST of the symbolic execution on the
-- query formula. This means we need a unit value to represent terms that have
-- been partially evaluated already
data BValue d = B! S.SBool
              | Unit
              | C! (Dim d) (SBVProp d) (SBVProp d)
              | BVOp! (BValue d) BB_B (BValue d)
              deriving (Eq, Show)

-- | given a BValue, we assume that the input has gone through
-- evaluation/accumulation. Count the terms in the core
vCoreSize :: BValue d -> Int
vCoreSize (BVOp l _ r) = vCoreSize l + vCoreSize r
vCoreSize _            = 1

-- | Count the plain terms, in the vcore
vCoreNumPlain :: BValue d -> Int
vCoreNumPlain (C _ _ _)    = 0
vCoreNumPlain (BVOp l _ r) = vCoreNumPlain l + vCoreNumPlain r
vCoreNumPlain _            = 1

-- | Count the variational terms, in the vcore
vCoreNumVar :: BValue d -> Int
vCoreNumVar (C _ _ _)    = 1
vCoreNumVar (BVOp l _ r) = vCoreNumVar l + vCoreNumVar r
vCoreNumVar _            = 0

vCoreMetrics :: ReadableProp Text -> IO (Int, Int, Int)
vCoreMetrics p = S.runSMT $ do
    p' <- St.evalStateT (propToSBool p) emptyPackState
    core <- evaluate $ toBValue p'
    return (vCoreSize core, vCoreNumPlain core, vCoreNumVar core)

-- | The main solver algorithm. You can think of this as the sem function for
-- the dsl. This progress in two stages, we extend the value domain with a
-- choice constructor to ensure that choices are evaluated absolutely last. If
-- they are not evaluated last then they will lose information from the rest of
-- the formula. For example if you have: ((c and d) and AA<-a, b>) and a and e,
-- then if you do simple recursion you'll generate a model when you evaluate the
-- choice, which occurs __before__ you evaluate the (a and e), therefore you'll
-- miss a unsatisfiable model. To work around this we evaluate all expression
-- around choices and inside the choices. When we see a choice we pause
-- evaluation and evaluate around and inside the choice, keeping a reference to
-- the result of the choices sibling in the AST. Once we compile down to the
-- value level we evaluate the choices properly by manipulating the assertion
-- stack, resulting in model generation and sbools
toBValue :: Resultable d => VProp d (S.SBool, Name) SNum -> BValue d
toBValue (RefB (b,name)) = B b
toBValue (LitB b) = B (S.literal b)
toBValue (OpB Not (OpB Not notchc)) = toBValue notchc
toBValue (OpB Not e) = driveNotDown e
toBValue (OpBB op (RefB (b,n)) (RefB (b',n'))) = (B bres)
  where bres = (bDispatch op) b b'
toBValue (OpBB op (ChcB d l r) r') = BVOp (C d l r) op (toBValue r')
toBValue (OpBB op l' (ChcB d l r)) = BVOp (toBValue l') op (C d l r)
toBValue (OpBB op l r) = BVOp (toBValue l) op (toBValue r)
toBValue (OpIB _ _ _) = error "Blame Jeff! This isn't implemented yet!"
toBValue (ChcB d l r) = C d l r

dbg :: (Show a, MonadIO m) => Text -> a -> m ()
dbg s a = liftIO $ T.putStrLn (s <> " : " <> toText a <> " \n")

-- | Evaluation allows communication with the solver and reduces terms to Unit
-- values thereby representing that evaluation has taken place. Evaluation can
-- call a switch into the accumulation mode in order to partially evaluate an
-- expression
evaluate :: (Resultable d, Ts.MonadSymbolic m, I.SolverContext m) =>
  BValue d -> m (BValue d)
evaluate Unit        = return Unit                             -- [Eval-Unit]
evaluate (B b) =
  -- trace "Eval B" $
  do Ts.constrain b; return Unit           -- [Eval-Term]
evaluate x@(C _ _ _) = return x                             -- [Eval-Chc]
-- evaluate (BVOp Unit _ Unit) = trace "double unit" $ return Unit                     -- [Eval-UAndR]
evaluate (BVOp Unit _ r) = evaluate r                     -- [Eval-UAndR]
evaluate (BVOp l _ Unit) = evaluate l                     -- [Eval-UAndL]

evaluate (BVOp l And x@(B _)) = do _ <- evaluate x; evaluate l
evaluate (BVOp x@(B _) And r) = do _ <- evaluate x; evaluate r

evaluate (BVOp l op x@(C _ _ _)) =
  -- trace ("recursive RChc with: \n") $
  do l' <- accumulate l
     let !res = BVOp l' op x
     if isValue l' && op == And
       then evaluate res
       else return res

evaluate (BVOp x@(C _ _ _) op r) =
  -- trace ("recursive LChc \n") $
  do r' <- accumulate r
     let !res = BVOp x op r'
     if isValue r' && op == And
       then evaluate res
       else return res

  -- evaluation of two bools, the case that does the actual work
evaluate (BVOp (B bl) op (B br)) =                     -- [Eval-Bools]
  -- trace "contracting" $
  evaluate (B res)
  where !res = bDispatch op bl br
        -- !name = ln ++ (pure $ toText op) ++ rn

evaluate (BVOp l And r) =                                      -- [Eval-And]
  -- trace ("recursive AND case: \n") $
  do l' <- evaluate l
     r' <- evaluate r
     let !res = BVOp l' And r'
     -- trace ("recursive AND case GOT RES:\n") $ return ()
     if isValue l' || isValue r'
       then evaluate res
       else return res

evaluate (BVOp l op r) =                                         -- [Eval-Or]
  -- trace ("recursive GEn case: \n") $
  do l' <- accumulate l
     r' <- accumulate r
     let !res = BVOp l' op r'
     -- trace ("recursive GEN case: \n") $ return ()
     if isValue l' && isValue r'
       then evaluate res
       else return res

accumulate :: (Resultable d, Ts.MonadSymbolic m) => BValue d -> m (BValue d)
accumulate Unit        = return Unit                             -- [Acc-Unit]
accumulate (x@(B _))   = return x                                -- [Acc-Term]
accumulate (x@(C _ _ _)) =
  -- trace "Ac: singleton chc" $
  return x                                -- [Acc-Chc]
accumulate (x@(BVOp (C _ _ _) _ (C _ _ _))) =  return x          -- [Acc-Op-ChcL]
accumulate (x@(BVOp (C _ _ _) _ (B _)))   =  return x          -- [Acc-Op-ChcL]
accumulate (x@(BVOp (B _) _ (C _ _ _)))   =  return x          -- [Acc-Op-ChcR]

-- accumulate (BVOp Unit _ Unit) = trace "double unit" $ return Unit
accumulate (BVOp Unit _ r) =
  -- trace ("AC LUNIT") $
  accumulate r                     -- [Acc-UAndR]
accumulate (BVOp l _ Unit) =
  -- trace ("Ac RUnit") $
  accumulate l                     -- [Acc-UAndL]

accumulate (BVOp l op x@(C _ _ _)) =
  -- trace "AC RCHC" $
  do l' <- accumulate l; return $! BVOp l' op x

accumulate (BVOp x@(C _ _ _) op r) =
  -- trace "AC LCHC" $
  do r' <- accumulate r; return $! BVOp x op r'

  -- accumulation of two bools
accumulate (BVOp (B bl) op (B br)) =                    -- [Acc-Bools]
  -- trace "Ac: Contracting bools" $
  return (B res)
  where !res = (bDispatch op bl br)
        -- !name = ln ++ (pure $ toText op) ++ rn

accumulate (BVOp l op r) =                                       -- [Acc-Or]
  -- trace ("Accum recursive case") $
  do l' <- accumulate l;
     r' <- accumulate r;
     let !res = BVOp l' op r'
     if isValue l' && isValue r'
       then accumulate res
       else return res

-- | Given a bvalue, return a symbolic reference of the negation of that bvalue
driveNotDown :: Resultable d => VProp d (S.SBool, Name) SNum -> BValue d
driveNotDown (LitB b)       = (B . bnot . S.literal $ b)
driveNotDown (RefB (b,_))   = (B $! bnot b)
driveNotDown (ChcB d l r)   = (C d (bnot l) (bnot r))                --[Acc-Neg-Chc]
driveNotDown (OpB Not e)    = toBValue e
driveNotDown (OpBB And l r) = BVOp l' Or r'
  where l' = driveNotDown l
        r' = driveNotDown r
driveNotDown (OpBB Or l r) = BVOp l' And r'
  where l' = driveNotDown l
        r' = driveNotDown r
driveNotDown (OpBB XOr l r)  = driveNotDown prop
  where prop = OpBB Or (OpBB And l (bnot r)) (OpBB And (bnot l) r)
driveNotDown (OpBB Impl l r) = driveNotDown (OpBB Or l' r)
  where l' = OpB Not l
driveNotDown (OpBB BiImpl l r) = driveNotDown prop
  where prop = (OpBB Or (OpBB And l r) (OpBB And (bnot l) (bnot r)))
driveNotDown (OpIB _ _ _) = error "Not implemented yet!"

isValue :: BValue d -> Bool
isValue (B _) = True
isValue Unit    = True
isValue _       = False

-- | After partial evaluation we need to use a zipper to find the most shallow
-- choice and begin evaluating choices. Ctx is the zipper
data Ctx d = InL (Ctx d) BB_B (BValue d)
           | InR !S.SBool BB_B (Ctx d)
           | Top
           deriving Show

type Loc d = (BValue d, Ctx d)

doChoice :: Loc Text -> IncVSMTSolve ()
doChoice (b@(B b'), Top) =
  -- trace ("Bool" ++ show b ++ "\n") $
  do evaluate b; solveVariant (return ())
doChoice (Unit, Top)     =
  -- trace ("Unit Top \n") $
  solveVariant $ return ()
doChoice x@(C d l r, Top) =
  -- trace ("Choice Top"  ++ show x ++ "\n") $
  do let
      bl = toBValue l
      goLeft = doChoice (bl, Top)

      br = toBValue r
      goRight = doChoice (br, Top)

     handleChc goLeft goRight d

doChoice (Unit, c@(InL parent op r)) =
  -- trace ("Unit L " ++ show c ++ "\n") $
  doChoice (r, parent)
doChoice (Unit, c@(InR acc op parent)) =
  -- trace ("Unit R " ++ show c ++ "\n") $
  doChoice (B acc, parent)

  -- when we see two bools we can evaluate them and climb the tree
doChoice (bl@(B _), InL parent op br@(B _)) =
  -- trace ("Accumulate case L \n") $
  accumulate (BVOp bl op br) >>= doChoice . mkCtx parent

doChoice (br@(B _), InR acc op parent) =
  -- trace ("Accumulate case R \n") $
  accumulate (BVOp (B acc) op br) >>= doChoice . mkCtx parent
  -- when we reach a bool leaf we set it as the accumulate and switch to the
  -- right fold
doChoice (B bl, InL parent op r) =
  -- trace ("Changing to fold case\n") $
  doChoice (r, InR bl op parent)

  -- when we find a choice we solve it recursively
doChoice x@(C d l r, ctx@(InL _ _ _))  =
  -- trace ("Got here L " ++ show x ++ "\n") $
  do let
      goLeft =
        -- bl <- evaluate (toBValue l)
        doChoice (toBValue l, ctx)

      goRight =
        -- br <- evaluate (toBValue r)
        doChoice (toBValue r, ctx)

     handleChc goLeft goRight d

doChoice x@(C d l r, ctx@(InR _ _ _)) =
  -- trace ("Got here R" ++ show x ++ "\n") $
  do
    let
      bl = toBValue l
      goLeft = doChoice (bl, ctx)

      br = toBValue r
      goRight = doChoice (br, ctx)

    handleChc goLeft goRight d

  -- we recur to the left in order to fold to the right
doChoice (BVOp l op r, ctx) =
  -- trace ("Recursive match\n") $
  doChoice (l, InL ctx op r)

mkTop :: BValue d -> Loc d
mkTop = mkCtx Top

mkCtx :: Ctx d -> BValue d -> Loc d
mkCtx = flip (,)

getFocus :: S.SBool -> Ctx d -> Loc d
getFocus b Top                     = (B b, Top)
getFocus b (InL parent op rbranch) = (rbranch, InR b op parent)
getFocus b (InR acc op Top)        = (B $! bDispatch op acc b, Top)
getFocus b (InR acc op (InL parent op' rbranch)) = (rbranch, InR res op' parent)
  where !res = bDispatch op acc b
getFocus b (InR acc op (InR acc' op' parent)) = (B res', parent)
  where !res = bDispatch op acc b
        !res' = bDispatch op' acc' res

-- | Given a Loc find the first choice by crawling down the left hand of the zipper
findPrincipleChoice :: Loc d -> Loc d
  -- base casee
findPrincipleChoice x@(C _ _ _, _)   = x
  -- terminal cases we want to avoid
findPrincipleChoice (BVOp (B x) op r, ctx) =
  -- trace "princ choice op" $
  findPrincipleChoice (r, InR x op ctx)
findPrincipleChoice (BVOp Unit op r, ctx) =
  -- trace "princ choice op unit" $
  findPrincipleChoice (r, InR true op ctx)
  -- recursive cases
findPrincipleChoice (BVOp l op r, ctx) =
  -- trace "princ choice op in L" $
  findPrincipleChoice (l, InL ctx op r)
  -- totalize over the context if all these fail and let doChoice handle it
findPrincipleChoice x = x

module Config where

import Data.SBV.Control (SMTOption(..))
import Data.SBV (SMTConfig(..),z3,yices,mathSAT,boolector,abc,cvc4)
import GHC.Generics (Generic)
import Data.Foldable (foldr')
import Data.Text (Text)

import VProp.Types
import Opts
import SAT

data Settings = Settings { solver         :: Solver
                         , optimizations  :: ![Opts]
                         , seed           :: Maybe Integer
                         , generateModels :: Bool
                         } deriving (Show,Generic)

data SMTConf d a b = SMTConf { conf :: !SMTConfig
                             , opts :: ![VProp d a b -> VProp d a b]
                             , settings :: Settings
                             }

type ReadableSMTConf d = SMTConf d Text Text

data Solver = Z3
            | Yices
            | MathSat
            | Boolector
            | Abc
            | Cvc4
            deriving (Show,Generic)

applyOpts :: SMTConf d a b -> VProp d a b -> VProp d a b
applyOpts conf p = foldr' ($) p (opts conf)

-- | Convert an interfacial interface to an SMT one
toConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => Settings -> SMTConf d a b
toConf Settings{..} = foldr' ($) emptyConf ss
  where ss = [setSeed seed, setSolver solver, setOpts optimizations, setModelGen generateModels]

-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{solver=Z3, optimizations=defs, seed=Nothing, generateModels = True}
  where defs = [Prune, Atomize, MoveRight]

defSettingsOnlySat :: Settings
defSettingsOnlySat = defSettings{generateModels = False}

-- moveRight required for proper results
minSettings :: Settings
minSettings = Settings{solver=Z3, optimizations=[], seed=Nothing, generateModels = False}

allOptsSettings :: Settings
allOptsSettings = defSettings{optimizations=[Atomize,Prune,Shrink,MoveRight]}

debugSettings :: Settings
debugSettings = Settings{ solver=Z3
                        , optimizations=[MoveRight,Atomize]
                        , seed=Nothing
                        , generateModels=True}

defConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
defConf = toConf defSettings

defConfOnlySat :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
defConfOnlySat = toConf defSettingsOnlySat

emptyConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
emptyConf = SMTConf{conf=z3, opts=[],settings=defSettings}

debugConf :: (SAT (VProp d a b),Ord a,Ord b,Ord d) => SMTConf d a b
debugConf = setVerbose $ toConf debugSettings

minConf ::(SAT (VProp d a b),Ord a,Ord b,Ord d) => SMTConf d a b
minConf = toConf minSettings

allOptsConf ::(SAT (VProp d a b),Ord a,Ord b,Ord d) => SMTConf d a b
allOptsConf = toConf allOptsSettings

-- | apply some function on the solver options. This could be done more cleanly
-- with lenses but I don't want to bloat the library
addOption :: ([SMTOption] -> [SMTOption]) -> SMTConf d a b -> SMTConf d a b
addOption f c = SMTConf { conf = c'{solverSetOptions=f sOpts}
                        , opts = opts c
                        , settings = settings c
                        }
  where c' = conf c
        sOpts = solverSetOptions c'

-- | set the seed of the internal solver
setSeed :: Maybe Integer -> SMTConf d a b -> SMTConf d a b
setSeed (Just x) c = addOption (RandomSeed x:) c
setSeed Nothing  c = c


setVerbose :: SMTConf d a b -> SMTConf d a b
setVerbose SMTConf{..} = SMTConf{conf=conf{verbose=True}, opts,settings}

setSolver :: Solver -> SMTConf d a b -> SMTConf d a b
setSolver Z3 a        = a{conf=z3}
setSolver Yices a     = a{conf=yices}
setSolver MathSat a   = a{conf=mathSAT}
setSolver Boolector a = a{conf=boolector}
setSolver Abc a       = a{conf=abc}
setSolver Cvc4 a      = a{conf=cvc4}

setOpts :: (Ord a,Ord b,Ord d, SAT (VProp d a b)) =>
  [Opts] -> SMTConf d a b -> SMTConf d a b
setOpts os c = c{opts=convertOpts <$> os}

setModelGen :: Bool -> SMTConf d a b -> SMTConf d a b
setModelGen b SMTConf{..} = SMTConf{ conf
                                   , opts
                                   ,settings=settings{generateModels = b}}

convertOpts :: (SAT (VProp d a b), Ord a, Ord b, Ord d) =>
  Opts -> VProp d a b -> VProp d a b
convertOpts MoveRight = chcToRight
convertOpts MoveLeft  = chcToLeft
convertOpts Shrink    = shrinkProp
convertOpts Prune     = prune
-- convertOpts CNF       = toCNF
convertOpts Atomize   = atomize
convertOpts _         = id


z3DefConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
z3DefConf = setSolver Z3 defConf

z3DefConfOnlySat :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
z3DefConfOnlySat = setSolver Z3 defConfOnlySat

yicesDefConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
yicesDefConf = setSolver Yices defConf

mathSatDefConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
mathSatDefConf = setSolver MathSat defConf

boolectorDefConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
boolectorDefConf = setSolver Boolector defConf

cvc4DefConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
cvc4DefConf = setSolver Cvc4 defConf

abcDefConf :: (SAT (VProp d a b), Ord a,Ord b,Ord d) => SMTConf d a b
abcDefConf = setSolver Abc defConf

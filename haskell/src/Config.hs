module Config where

import Data.SBV.Control (SMTOption(..))
import Data.SBV (SMTConfig(..),z3,yices,mathSAT,boolector,abc,cvc4)
import GHC.Generics (Generic)
import Data.Foldable (foldr')

import VProp.Types
import Opts

data Settings = Settings { solver :: Solver
                         , optimizations :: ![Opts]
                         , seed :: Maybe Integer
                         } deriving (Show,Generic)

data SMTConf d a b = SMTConf { conf :: !SMTConfig
                             , opts :: ![VProp d a b -> VProp d a b]
                             }

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
toConf :: (Show a,Show d,Show b,Ord a,Ord b,Ord d) => Settings -> SMTConf d a b
toConf Settings{..} = foldr' ($) emptyConf ss
  where ss = [setSeed seed, setSolver solver, setOpts optimizations]

-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{solver=Z3, optimizations=defs, seed=Nothing}
  where defs = [Prune, Atomize, MoveRight]

-- moveRight required for proper results
minSettings :: Settings
minSettings = Settings{solver=Z3, optimizations=defs, seed=Nothing}
  where defs = [MoveRight]

allOptsSettings :: Settings
allOptsSettings = Settings{ solver=Z3
                          , optimizations=[Atomize,Prune,Shrink,MoveRight]
                          , seed=Nothing}

debugSettings :: Settings
debugSettings = Settings{ solver=Z3
                        , optimizations=[MoveRight,Atomize]
                        , seed=Nothing}

defConf :: (Show a,Show d,Show b,Ord a,Ord b,Ord d) => SMTConf d a b
defConf = toConf defSettings

emptyConf :: (Show a,Show d,Show b,Ord a,Ord b,Ord d) => SMTConf d a b
emptyConf = SMTConf{conf=z3, opts=[]}

debugConf :: (Show a,Show d,Show b,Ord a,Ord b,Ord d) => SMTConf d a b
debugConf = setVerbose $ toConf debugSettings

minConf ::(Show a,Show d,Show b,Ord a,Ord b,Ord d) => SMTConf d a b
minConf = toConf minSettings

allOptsConf ::(Show a,Show d,Show b,Ord a,Ord b,Ord d) => SMTConf d a b
allOptsConf = toConf allOptsSettings

-- | apply some function on the solver options. This could be done more cleanly
-- with lenses but I don't want to bloat the library
addOption :: ([SMTOption] -> [SMTOption]) -> SMTConf d a b -> SMTConf d a b
addOption f c = SMTConf {conf = c'{solverSetOptions=f sOpts}, opts = os}
  where c' = conf c
        sOpts = solverSetOptions c'
        os = opts c

-- | set the seed of the internal solver
setSeed :: (Maybe Integer) -> SMTConf d a b -> SMTConf d a b
setSeed (Just x) c = addOption ((RandomSeed x):) c
setSeed Nothing  c = c


setVerbose :: SMTConf d a b -> SMTConf d a b
setVerbose SMTConf{..} = SMTConf{conf=conf{verbose=True}, opts}

setSolver :: Solver -> SMTConf d a b -> SMTConf d a b
setSolver Z3 a        = a{conf=z3}
setSolver Yices a     = a{conf=yices}
setSolver MathSat a   = a{conf=mathSAT}
setSolver Boolector a = a{conf=boolector}
setSolver Abc a       = a{conf=abc}
setSolver Cvc4 a      = a{conf=cvc4}

setOpts :: (Ord a,Ord b,Ord d,Show d,Show b,Show a) =>
  [Opts] -> SMTConf d a b -> SMTConf d a b
setOpts os c = c{opts=convertOpts <$> os}

convertOpts :: (Ord a,Ord b,Ord d,Show d,Show a,Show b) =>
  Opts -> VProp d a b -> VProp d a b
convertOpts MoveRight = chcToRight
convertOpts MoveLeft  = chcToLeft
convertOpts Shrink    = shrinkProp
convertOpts Prune     = prune
-- convertOpts CNF       = toCNF
convertOpts Atomize   = atomize
convertOpts _         = id

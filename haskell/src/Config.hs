module Config where

import Data.SBV.Control (SMTOption(..))
import Data.SBV (SMTConfig(..),z3,yices,mathSAT,boolector,abc,cvc4)
import GHC.Generics (Generic)
import Data.Foldable (foldr')

import VProp.Types
import Opts

data Settings = Settings { solver :: Solver
                         , optimizations :: [Opts]
                         , seed :: Maybe Integer
                         } deriving (Show,Generic)

data SMTConf a = SMTConf { conf :: SMTConfig
                         , opts :: [VProp a a -> VProp a a]
                         }

data Solver = Z3
            | Yices
            | MathSat
            | Boolector
            | Abc
            | Cvc4
            deriving (Show,Generic)

-- | Convert an interfacial interface to an SMT one
toConf :: (Ord a, Show a) => Settings -> SMTConf a
toConf Settings{..} = foldr' ($!) defConf ss
  where ss = [setSeed seed, setSolver solver, setOpts optimizations]

-- | A default configuration uses z3 and tries to shrink propositions
defSettings :: Settings
defSettings = Settings{solver=Z3, optimizations=defs, seed=Nothing}
  where defs = [MoveLeft, Atomize, CNF, Prune]

allOptsSettings :: Settings
allOptsSettings = Settings{ solver=Z3
                          , optimizations=[MoveLeft,Shrink]
                          , seed=Nothing}

defConf :: (Ord a,Show a) => SMTConf a
defConf = SMTConf{conf=z3, opts=[moveChcToLeft, shrinkProp]}

allOptsConf :: (Ord a,Show a) => SMTConf a
allOptsConf = toConf allOptsSettings

-- | apply some function on the solver options. This could be done more cleanly
-- with lenses but I don't want to bloat the library
addOption :: ([SMTOption] -> [SMTOption]) -> SMTConf a -> SMTConf a
addOption f c = SMTConf {conf = c'{solverSetOptions=f sOpts}, opts = os}
  where c' = conf c
        sOpts = solverSetOptions c'
        os = opts c

-- | set the seed of the internal solver
setSeed :: (Maybe Integer) -> SMTConf a -> SMTConf a
setSeed (Just x) c = addOption ((RandomSeed x):) c
setSeed Nothing  c = c

setSolver :: Solver -> SMTConf a -> SMTConf a
setSolver Z3 a = a{conf=z3}
setSolver Yices a = a{conf=yices}
setSolver MathSat a = a{conf=mathSAT}
setSolver Boolector a = a{conf=boolector}
setSolver Abc a = a{conf=abc}
setSolver Cvc4 a = a{conf=cvc4}

setOpts :: (Ord a, Show a) => [Opts] -> SMTConf a -> SMTConf a
setOpts os c = c{opts=convertOpts <$> os}

convertOpts :: (Ord a,Show a) =>  Opts -> VProp a a -> VProp a a
convertOpts MoveRight = moveChcToRight
convertOpts MoveLeft  = moveChcToLeft
convertOpts Shrink    = shrinkProp
convertOpts Prune     = prune
convertOpts CNF       = toCNF
convertOpts Atomize   = atomize
convertOpts _         = id

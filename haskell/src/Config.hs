module Config where

import Data.SBV.Control (SMTOption(..))
import Data.SBV (SMTConfig(..),z3)

import VProp.Types
import Opts (shrinkProp)

data SMTConf a = SMTConf { conf :: SMTConfig
                         , opts :: [VProp a a -> VProp a a]
                         }

-- | A default configuration uses z3 and tries to shrink propositions
defConf :: (Show a, Ord a) => SMTConf a
defConf = SMTConf{conf=z3, opts=[shrinkProp]}

-- | apply some function on the solver options. This could be done more cleanly
-- with lenses but I don't want to bloat the library
addOption :: ([SMTOption] -> [SMTOption]) -> SMTConf a -> SMTConf a
addOption f c = SMTConf {conf = c'{solverSetOptions=f sOpts}, opts = os}
  where c' = conf c
        sOpts = solverSetOptions c'
        os = opts c

-- | set the seed of the internal solver
setSeed' :: Integer -> SMTConf a -> SMTConf a
setSeed' x = addOption ((:) (RandomSeed x))

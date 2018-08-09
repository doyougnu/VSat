module Config where

import Data.SBV.Control (SMTOption(..))
import Data.SBV (SMTConfig(..),z3)

import VProp.Types
import Opts (shrinkProp)

data SMTConf a = SMTConf { conf :: SMTConfig
                         , opts :: [VProp a a -> VProp a a]
                         }

defConf :: (Show a, Ord a) => SMTConf a
defConf = SMTConf{conf=z3, opts=[shrinkProp]}

setSeed :: Integer -> SMTConf a -> SMTConf a
setSeed x c = SMTConf {conf = c'{solverSetOptions=(RandomSeed x : sOpts)}
                      , opts=os}
  where c' = conf c
        sOpts = solverSetOptions c'
        os = opts c

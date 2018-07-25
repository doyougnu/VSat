module Json ( module Data.Aeson) where

import V (V(..))
import Data.SBV (SatResult(..), SMTResult(..),ThmResult(..),SMTConfig(..),SMTSolver(..),Solver(..))
import Data.SBV.Internals (showModel)
import Control.Monad (liftM2)

import Data.Text
import Data.Aeson

instance ToJSON SMTResult where
  toJSON (Unsatisfiable _) = object [("isSat" :: Text) .= ("Unsatisfiable" :: Text)]
  toJSON (Satisfiable conf model) =
    object [("model" :: Text) .= showModel conf model]
  toJSON (SatExtField conf model) =
    object [("extModel" :: Text) .= showModel conf model]
  toJSON (Unknown _ msg) = object [("Unknown Error" :: Text) .= msg]
  toJSON (ProofError _ msg) = object [("Prover Error" :: Text) .= msg]


instance ToJSON SatResult where toJSON (SatResult x) = toJSON x
instance ToJSON ThmResult where toJSON (ThmResult x) = toJSON x

instance (Show d, Show a, ToJSON a, ToJSON d) => ToJSON (V a d) where
  toJSON (Plain x) = toJSON x
  toJSON (VChc d l r) = object [ (pack (show d) :: Text) .=
                                 object [ ("true" :: Text) .= toJSON l
                                        , ("false" :: Text) .= toJSON r
                                        ]
                               ]

instance Eq SMTResult where
  (Unsatisfiable x) == (Unsatisfiable y) = x == y
  (Satisfiable _ x) == (Satisfiable _ y) = x == y
  (Satisfiable _ x) == (Satisfiable _ y) = x == y

instance Eq Solver where
  Z3        == Z3        = True
  Yices     == Yices     = True
  Boolector == Boolector = True
  CVC4      == CVC4      = True
  MathSAT   == MathSAT   = True
  ABC       == ABC       = True
  _         == _         = False

instance Eq SMTConfig where
  (SMTConfig
   {verbose             = a
   ,timing              = b
   ,printBase           = c
   ,printRealPrec       = d
   ,satCmd              = e
   ,allSatMaxModelCount = f
   ,isNonModelVar       = g
   ,transcript          = h
   ,smtLibVersion       = i
   ,solver              = j
   ,roundingMode        = k
   ,solverSetOptions    = l
   ,ignoreExitCode      = m
   ,redirectVerbose     = n
   }) == (SMTConfig{..}) = a == verbose &&
                           c == printBase &&
                           d == printRealPrec &&
                           e == satCmd &&
                           (==) f allSatMaxModelCount &&
                           (==) h transcript &&
                           (name j) == (name solver) &&
                           (==) n redirectVerbose

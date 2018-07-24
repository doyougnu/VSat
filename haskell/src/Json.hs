module Json ( module Data.Aeson) where

import V (V(..))
import Data.SBV (SatResult(..), SMTResult(..),ThmResult(..))
import Data.SBV.Internals (showModel)

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

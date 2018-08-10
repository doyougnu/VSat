module Json where

import Data.SBV ( SatResult(..)
                , SMTResult(..)
                , ThmResult(..))
import Data.SBV.Internals (showModel)

import Data.Text
import Data.Aeson

import V (V(..))
import VProp.Types
import VProp.Core

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
                                 object [ ("L" :: Text) .= toJSON l
                                        , ("R" :: Text) .= toJSON r
                                        ]
                               ]

-- | VIExpr instances for FromJSON
instance FromJSON NPrim where
  parseJSON = withObject "num" $ \x -> do
    type' <- x .: "type"
    case type' of
      "I" -> I <$> x .: "value"
      "D" -> D <$> x .: "value"
      _   -> fail ("unknown numeric type: " ++ type')

instance FromJSON N_N
instance FromJSON B_B
instance FromJSON NN_N
instance FromJSON BB_B
instance FromJSON NN_B
instance FromJSON RefN
instance FromJSON Opn
instance FromJSON Dim
instance FromJSON a => FromJSON (VIExpr a)

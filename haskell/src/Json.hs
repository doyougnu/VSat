module Json where

import Data.SBV ( SatResult(..)
                , SMTResult(..)
                , ThmResult(..)
                , SMTReasonUnknown(..))
import Data.SBV.Internals (showModel)
import Data.Text
import Data.Aeson hiding (json)

import V (V(..))
import VProp.Types
import qualified Result as R
import Opts
import Config

instance ToJSON SMTResult where
  toJSON (Unsatisfiable _ _) = object [("isSat" :: Text) .= ("Unsatisfiable" :: Text)]
  toJSON (Satisfiable conf model) =
    object [("model" :: Text) .= showModel conf model]
  toJSON (SatExtField conf model) =
    object [("extModel" :: Text) .= showModel conf model]
  toJSON (Unknown _ msg) = object [("Unknown Error" :: Text) .= msg]
  toJSON (ProofError _ msg) = object [("Prover Error" :: Text) .= msg]

instance ToJSON SMTReasonUnknown
instance ToJSON SatResult where toJSON (SatResult x) = toJSON x
instance ToJSON ThmResult where toJSON (ThmResult x) = toJSON x
instance (R.Resultable d, ToJSONKey d, ToJSON d) => ToJSON (R.Result d)
instance (R.Resultable d, ToJSONKey d, ToJSON d) => ToJSON (R.ResultMap d)
instance (R.Resultable d, FromJSONKey d, FromJSON d) => FromJSON (R.Result d)
instance (R.Resultable d, FromJSONKey d, FromJSON d) => FromJSON (R.ResultMap d)
instance FromJSON d => FromJSON (R.UniformProp d)
instance ToJSON d => ToJSON (R.UniformProp d)
instance FromJSON d => FromJSON (R.ResultProp d)
instance ToJSON d => ToJSON (R.ResultProp d)

instance (Show d, Show a, ToJSON a, ToJSON d) => ToJSON (V a d) where
  toJSON (Plain x) = toJSON x
  toJSON (VChc d l r) = object [ (pack (show d) :: Text) .=
                                 object [ ("L" :: Text) .= toJSON l
                                        , ("R" :: Text) .= toJSON r
                                        ]
                               ]

-- Just keeping this for reference on writing a manual instance if needed later
-- | VIExpr instances for FromJSON
-- Test With: decode "{\"type\":\"I\", \"value\": 1000}" :: Maybe NPrim
-- instance FromJSON NPrim where
--   parseJSON = withObject "num" $ \x -> do
--     type' <- x .: "type"
--     case type' of
--       "I" -> I <$> x .: "value"
--       "D" -> D <$> x .: "value"
--       _   -> fail ("unknown numeric type: " ++ type')


instance FromJSON N_N
instance FromJSON NPrim
instance FromJSON B_B
instance FromJSON NN_N
instance FromJSON BB_B
instance FromJSON NN_B
instance FromJSON RefN
instance FromJSON Var
instance (FromJSON a) => FromJSON (Dim a)
instance (FromJSON d, FromJSON a) => FromJSON (VIExpr d a)
instance (FromJSON d, FromJSON a, FromJSON b) => FromJSON (VProp d a b)

instance ToJSON B_B
instance ToJSON NN_N
instance ToJSON N_N
instance ToJSON NPrim
instance ToJSON BB_B
instance ToJSON NN_B
instance ToJSON RefN
instance (ToJSON a) => ToJSON (Dim a)
instance ToJSON Var
instance ToJSONKey Var

instance (ToJSON a, ToJSON d) => ToJSON (VIExpr d a)
instance (ToJSON d, ToJSON a, ToJSON b) => ToJSON (VProp d a b)

instance ToJSON Opts
instance FromJSON Opts

instance FromJSON Solver
instance ToJSON Solver
instance FromJSON Settings
instance ToJSON Settings

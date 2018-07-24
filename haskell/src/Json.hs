module Json ( module Data.Aeson) where

import V
import Run (Result)
import Data.SBV (SatResult(..), SMTResult(..),ThmResult(..))
import Data.SBV.Internals (SMTModel, showModel)

import Data.Text
import Data.Aeson


-- [Plain (Just Satisfiable. Model:
--   uvtkzmyevlwnjgnlrpqbhuwqolrgzs = False :: Bool
--   gdbulfwfqyhunejbhxvlfsrkfidzkx = False :: Bool
--   egqnikeuyt                     = False :: Bool
--   yxttobxccxhlkdypxwskzdm        = False :: Bool
--   ozojlitgquueborcdxafjpxoscutsn = False :: Bool
--   sedfvolsdlrhrysekdsjbs         = False :: Bool)
-- ,VChc (Dim {dimName = "BB"}) (Plain (Just Satisfiable. Model:
--   uvtkzmyevlwnjgnlrpqbhuwqolrgzs = False :: Bool
--   gdbulfwfqyhunejbhxvlfsrkfidzkx = False :: Bool
--   egqnikeuyt                     = False :: Bool
--   yxttobxccxhlkdypxwskzdm        = False :: Bool
--   ozojlitgquueborcdxafjpxoscutsn = False :: Bool
--   sedfvolsdlrhrysekdsjbs         = False :: Bool)) (Plain Nothing)
-- ,VChc (Dim {dimName = "AA"}) (Plain (Just Satisfiable. Model:
--   uvtkzmyevlwnjgnlrpqbhuwqolrgzs = False :: Bool
--   gdbulfwfqyhunejbhxvlfsrkfidzkx = False :: Bool
--   egqnikeuyt                     = False :: Bool
--   yxttobxccxhlkdypxwskzdm        = False :: Bool
--   ozojlitgquueborcdxafjpxoscutsn = False :: Bool
--   sedfvolsdlrhrysekdsjbs         = False :: Bool))
-- (Plain (Just Satisfiable. Model:
--   uvtkzmyevlwnjgnlrpqbhuwqolrgzs = False :: Bool
--   gdbulfwfqyhunejbhxvlfsrkfidzkx = False :: Bool
--   egqnikeuyt                     = False :: Bool
--   yxttobxccxhlkdypxwskzdm        = False :: Bool
--   ozojlitgquueborcdxafjpxoscutsn = False :: Bool
--   sedfvolsdlrhrysekdsjbs         = False :: Bool))]

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
-- instance ToJSON Run.Result

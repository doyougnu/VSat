module VProp.Json.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Aeson (encode, decode)
import Control.Monad (liftM2)
import Data.Maybe (maybe)

import VProp.Types
import VProp.Gen
import Json

jsonProperties :: TestTree
jsonProperties = testGroup "Json Serializing and Printing Properties" $
                 [ QC.testProperty "Json roundtripping always succeeds" $
                   jsonRoundTrip
                 ]

jsonRoundTrip :: VProp Var Var Var -> Bool
jsonRoundTrip x = maybe False (==x) . decode . encode $ x

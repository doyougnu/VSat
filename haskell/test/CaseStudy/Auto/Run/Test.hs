module CaseStudy.Auto.Run.Test where

import Test.Tasty
import Data.Text
import           Config
import Api
import qualified Test.Tasty.HUnit as H
import qualified Data.ByteString         as BS (readFile)
import Control.Monad.Trans (liftIO)
import qualified Test.QuickCheck.Monadic as QCM
import qualified Data.Map as M
import           Data.Either             (lefts, rights)
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           CaseStudy.Auto.Run
import           VProp.Core
import           VProp.SBV               (toPredicate)
import           VProp.Types
import           Text.Megaparsec         (parse)
import           Data.Aeson              (decodeStrict)

import Result
import VProp.Boolean

--------------------------- Constants -----------------------------------------
autoFileBool :: FilePath
autoFileBool = "bench/AutoBench/Automotive02_merged_evolution_history_boolean.json"
--------------------------- Constants -----------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ auto_model_correct
  ]

auto_model_correct :: TestTree
auto_model_correct = H.testCase
  "models from solving the auto dataset, when substituted, result in T"
  auto_model_correct'

auto_model_correct' :: H.Assertion
auto_model_correct' =
  do
    bJsn <- BS.readFile autoFileBool
    let
      (Just bAuto) = decodeStrict bJsn :: Maybe Auto
      !bCs = constraints bAuto
      bPs' = parse langParser "" <$> bCs
      bPs = fmap (simplifyCtxs . renameCtxs sameCtxs) $ rights bPs'

      -- | Hardcoding equivalencies in generated dimensions to reduce number of
      -- dimensions to 4
      sameDims :: Text -> Text
      sameDims d
        | d == "D_1" = "D_2"
        | d == "D_3" = "D_4"
        | otherwise = d

      bProp = ((renameDims sameDims) . naiveEncode . autoToVSat) $ autoAndJoin (Prelude.take 2000 bPs)

    model <- satWith emptyConf bProp
    cfgs <- deriveModels model
    let getRes c = solveLiterals $ substitute (M.toList $ deriveValues model c) (selectVariantTotal c bProp)
        -- values = deriveValues model cfg
        res = getRes <$> (Prelude.take 1 cfgs)
    liftIO . putStrLn $ "[VSAT]: \n" ++ show res
    H.assertBool ("Failed with") (Prelude.all (==True) res)

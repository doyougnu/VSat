module Run.Test where

import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.Tasty.Hspec as HS
import Data.SBV ( SatResult(..)
                , SMTResult(..)
                , ThmResult(..)
                , SMTConfig(..)
                , SMTSolver(..)
                , Solver(..)
                , Modelable(..))
import Data.SBV.Internals (showModel, SMTModel(..), CW)
import Data.SBV           (getModelDictionary)
import Data.List          (all)
import Control.Monad.Trans (liftIO)
import Data.Monoid (Sum)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM2, liftM)
import Data.Maybe (maybe, isJust, catMaybes)
import Data.Map   (keys, Map)
import Data.Char  (isLower)
import Data.Set   (Set)

import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen
import Config (defConf, allOptsConf, emptyConf)
import Run
import Result
import Api
import qualified V as V

import Debug.Trace (trace)

runProperties :: TestTree
runProperties = testGroup "Run Properties" [
  -- andDecomp_terminatesSh
  -- , sat_term
  -- sat_error
  -- dim_homo
  -- , sat_error2
  -- , sat_error3
  -- vsat_matches_BF_plain
  -- vsat_matches_BF
  -- no_dims_in_model


                                           -- ad_term2
                                           -- ad_term
                                           -- , qcProps
                                           ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [
  --   sat_error
  -- , sat_error2
  -- , sat_error4
  -- andDecomp_duplicate
  -- , andDecomp_duplicateChc
  -- , dim_homo'
  -- , dupDimensions
  -- , not_is_handled
  -- , singleton_is_sat
  -- , not_mult_is_handled
  -- , chc_singleton_is_sat
  -- , chc_not_singleton_is_sat
  -- , chc_unbalanced_is_sat
  chc_balanced_is_sat
  -- , chc_2_nested_is_sat
  -- , bimpl_w_false_is_sat
  -- , bimpl_w_false_chc_is_sat
  -- , mixed_and_impl_is_sat
    -- chces_not_in_model
  ]

specTests :: TestTree
specTests = unsafePerformIO $ HS.testSpec "simple spec" hspecTest

hspecTest :: HS.Spec
hspecTest = HS.describe "hs describe" $ do
  HS.it "it was found" $ do
    1 `HS.shouldBe` 1

sat_term = QC.testProperty
           "Satisfiability terminates on any input"
           sat_terminates

dim_homo = QC.testProperty
           "Dimensions are homomorphic over solving i.e. dimensions are preserved, always"
           dim_homomorphism

no_dims_in_model = QC.testProperty
                  "Running brute force never convolves object level variables with dimensions"
                  no_dims_in_model_prop

vsat_matches_BF = QC.testProperty
                  "VSat with an empty configuration always matches Brute Force results"
                  vsat_matches_BF'

vsat_matches_BF_plain = QC.testProperty
  "VSat with an empty configuration always matches Brute Force results for only plain props"
  vsat_matches_BF_plain'


dim_homo' = H.testCase
            "dim homomorphism for simplest nested case"
            dim_homo_unit

dupDimensions = H.testCase
                 "If we have duplicate dimensions on input, they are merged on output"
                 dupDimensions'

andDecomp_terminatesSh = QC.testProperty
                         "And decomp terminates with shared generated props"
                         ad_terminates

sat_error = H.testCase
           "Coercian with division works properly"
           sat_error_unit

sat_error2 = H.testCase
           "Inequality with doubles works properly"
           sat_error_unit2

sat_error3 = H.testCase
           "Modulus with doubles works"
           sat_error_unit3

sat_error4 = H.testCase
           "The solver doesn't run out of memory"
           sat_error_unit4

not_is_handled = H.testCase
                 "Negation doesn't immediately cause unsat for vsat routine"
                 not_unit

not_mult_is_handled = H.testCase
                 "Negation doesn't immediately cause unsat for vsat routine"
                 not_mult_unit

singleton_is_sat = H.testCase
                 "a single reference variable is always satisfiable"
                 singleton_unit

chc_singleton_is_sat = H.testCase
                       "a single choice of singletons is sat"
                       chc_singleton_unit

chc_not_singleton_is_sat = H.testCase
                       "a negated single choice of singletons is sat"
                       chc_singleton_not_unit

chc_unbalanced_is_sat = H.testCase
                       "if a choice is unbalanced the return model is also unbalanced"
                       chc_unbalanced_unit

chc_balanced_is_sat = H.testCase
                      "un-nested choices return a model that is a balanced tree"
                      chc_balanced_unit

chc_2_nested_is_sat = H.testCase
                      "nested choices return a model that is a balanced tree"
                      chc_2_nested_unit


bimpl_w_false_is_sat = H.testCase
                          "A bimplication with a False is always unsat"
                          bimpl_w_false_is_sat_unit

bimpl_w_false_chc_is_sat = H.testCase
                           "A bimplication with a False and a choice is always unsat"
                           bimpl_w_false_chc_is_sat_unit

mixed_and_impl_is_sat = H.testCase
                           "A bimplication with a False and a choice is always unsat"
                           mixed_and_impl_is_sat_unit

chces_not_in_model = H.testCase
                     "For brute for we never see choices in a returned model"
                     chces_not_in_model_unit

andDecomp_duplicate = H.testCase
  "And decomposition can solve props with repeat variables" $
  do a <- ad id prop
     H.assertBool "should never be empty" (not $ isDMNull a)
  where
    prop :: VProp Var Var Var
    prop = x ./= x
    x :: VIExpr Var Var
    x = iRef "x"

andDecomp_duplicateChc = H.testCase
  "And decomposition can solve props with repeat dimensions" $
  do a <- ad id prop
     H.assertBool "should never be empty" (not $ isDMNull a)
  where
    prop :: ReadableProp
    prop = ChcB "D" (bRef "c" &&& bRef "d") (bRef "a") &&& ChcB "D" (bRef "a") (bRef "c")

andDecomp_terminatesSh_ = QCM.monadicIO $
  do
    let gen = genVPropAtShare 5 $ vPropShare (repeat 4)
    prop <- QCM.run . QC.generate $ gen `QC.suchThat` onlyInts
    liftIO $ print "----\n"
    liftIO $ print prop
    liftIO $ print "----\n"
    a <- QCM.run $ ad id prop
    QCM.assert (not $ isDMNull a)

sat_terminates x =  onlyInts x QC.==> QCM.monadicIO
  $ do -- liftIO $ print $ "prop: " ++ show (x :: VProp Var Var) ++ " \n"
       a <- QCM.run . sat $ (x :: ReadableProp)
       QCM.assert (not $ isDMNull a)

vsat_matches_BF' x =  onlyBools x QC.==> QCM.monadicIO
  $ do a <- QCM.run . (bfWith emptyConf) $ (x :: VProp Var Var Var)
       b <- QCM.run . (satWith emptyConf) $ x
       liftIO . putStrLn $ "[BF]:   \n" ++ show a
       liftIO . putStrLn $ "[VSAT]: \n" ++ show b
       QCM.assert (a == b)

noDimsInModel :: V.V d (Maybe SatResult) -> Bool
noDimsInModel res = all (strIsLower) resultVars
  where
    resultModels = V.values' $ (fmap (keys . getModelDictionary)) <$> res

    resultVars :: [String]
    resultVars = mconcat $ catMaybes resultModels

    strIsLower :: String -> Bool
    strIsLower = all isLower

no_dims_in_model_prop x =  onlyBools x QC.==> QCM.monadicIO
  $ do a <- QCM.run . (bfWith emptyConf) $ (x :: ReadableProp)
       -- liftIO . putStrLn $ "[BF]:   \n" ++ show a'
       QCM.assert (mempty == getResSat a)

vsat_matches_BF_plain' x =
  (onlyBools x && isPlain x) QC.==> QCM.monadicIO
  $ do a <- QCM.run . (bfWith emptyConf) $ (x :: ReadableProp)
       b <- QCM.run . (satWith emptyConf) $ (x :: ReadableProp)
       liftIO . putStrLn $ "\n[BF]:   \n" ++ show a
       liftIO . putStrLn $ "[VSAT]: \n" ++ show b
       QCM.assert (a == b)

ad_terminates x = onlyInts x QC.==> QCM.monadicIO
  $ do -- liftIO $ print $ "prop: " ++ show (x :: VProp Var Var)
       -- liftIO $ print $ "prop Dup?: " ++ show (noDupRefs x)
       a <- QCM.run . ad id $ (x :: VProp Var Var Var)
       QCM.assert (not $ isDMNull a)

dim_homomorphism x = onlyInts x QC.==> QCM.monadicIO
  $ do a <- QCM.run . satWith emptyConf $ (x :: VProp Var Var Var)
       -- liftIO $ print $ "prop: " ++ show (x :: VProp Var Var)
       -- liftIO $ print $ "dims: " ++ show (dimensions x)
       -- liftIO $ print $ "num dims: " ++ show (length $ dimensions x)

       QCM.assert (length (dimensions x) == length (getProp $ getResSat a))

dim_homo_unit = do a <- satWith emptyConf prop
                   let numDimsAfter = length $ dimensions (getProp $ getResSat a)
                       numDimsBefore = length $ dimensions prop
                   print prop
                   print a

                   H.assertBool "" (numDimsBefore == numDimsAfter)
  where prop :: VProp Var Var Var
        prop = (ChcB "AA" (bRef "x") (bRef "y")) ==> (ChcB "DD" true false)


dupDimensions' = do a <- satWith emptyConf prop
                    let numDimsAfter = length $ bvars $ getProp $ lookupRes_ "__SAT" a
                        numDimsBefore = length $ dimensions prop

                    -- print numDimsBefore
                    -- print numDimsAfter
                    print prop
                    print a
                    H.assertBool "" (numDimsBefore >= numDimsAfter)
  where prop :: VProp Var Var Var
        prop = (ChcB "AA" (bRef "x") (bRef "y")) &&& ((bRef "z") ==> (ChcB "AA" true false))
        -- prop = (ChcB "AA" (bRef "x") (bRef "y")) &&& (ChcB "AA" true false)

sat_error_unit = do a <- sat prop
                    H.assertBool "" (not $ (==) mempty a)
  where
    prop :: VProp Var Var Var
    prop = (signum 7 - (LitI . D $ 10.905)) ./=
           ((signum (signum (dRef "x" :: VIExpr Var Var))) + signum 6)
    -- prop = (signum 7 - (LitI . D $ 10.905)) .== (0 :: VIExpr String)
    -- prop = (signum 7 - (LitI . D $ 10.905)) .== (0 :: VIExpr String)
    -- prop = ((dRef "x" - iRef "q") .== 0) &&& (bRef "w" &&& bRef "rhy")

sat_error_unit2 = do a <- sat prop
                     H.assertBool "" (not $ (==) mempty a)
  where
    prop :: VProp Var Var Var
    prop = ((dRef "x" :: VIExpr Var Var) .<= (LitI . D $ 15.309)) &&& true
    -- prop = (dRef "x") .== (LitI . I $ 15) -- this passes

sat_error_unit3 = do a <- sat prop
                     H.assertBool "Modulus with Doubles passes as long as there is one integer" . not . (==) mempty $ a
  where
    prop :: VProp Var Var Var
    prop = (dRef "x" :: VIExpr Var Var) .%
           (dRef "y" :: VIExpr Var Var) .> (LitI . I $ 1)

sat_error_unit4 = do a <- sat prop
                     H.assertBool "Division with a Double and Int coearces correctly" . not . (==) mempty $ a
  where
    prop :: VProp Var Var Var
  -- this will still fail with a bitvec error
    prop =  (dRef "x" :: VIExpr Var Var) .% (-6) ./= (-(LitI . D $ 74.257))
    -- prop =  (abs (iRef "x")) .% (-6) ./= (-(LitI . D $ 74.257)) -- this is the original error, a define_fun
    -- prop = ((abs (dRef "x")) .% (-6) ./= (-(LitI . D $ 74.257))) <+> (bnot (bRef "y")) -- this throws a bitvec error
    -- (|ogzpzgeb| .% -6 ≠ -74.25731844390708) ⊻ ¬opvp

unitGen prop str = do a <- satWith emptyConf prop
                      b <- bfWith emptyConf prop
                      putStrLn "\n\n--------------"
                      putStrLn $ show prop
                      putStrLn $ show b
                      putStrLn $ show a
                      putStrLn "--------------\n\n"
                      H.assertBool str (a == b)

not_unit = do a <- satWith emptyConf prop
              b <- bfWith emptyConf prop
              putStrLn $ show prop
              H.assertBool "Brute Force matches VSAT for simple negations" (a == b)
  where prop :: VProp Var Var Var
        prop = bnot . bRef $ "x"

not_mult_unit = do a <- satWith emptyConf prop
                   b <- bfWith emptyConf prop
                   putStrLn $ show prop
                   putStrLn $ show a
                   putStrLn $ show b
                   H.assertBool "Brute Force matches VSAT for multiple negations" (a == b)
  where prop :: VProp Var Var Var
        prop = bnot . bnot . bnot . bRef $ "x"

singleton_unit = do a <- satWith emptyConf prop
                    b <- bfWith emptyConf prop
                    putStrLn $ show prop
                    H.assertBool "Brute Force matches VSAT for simple negations" (a == b)
  where prop :: VProp Var Var Var
        prop = bRef $ "x"

chc_singleton_unit = unitGen prop "BF matches VSAT for a singleton choice of singletons"
  where
    prop :: ReadableProp
    prop = bChc "AA" (bRef "x") (bRef "y")

chc_singleton_not_unit = unitGen prop "BF matches VSAT for a negated singleton choice of singletons"
  where
    prop :: ReadableProp
    prop =  bnot $ bChc "AA" false true

chc_unbalanced_unit = unitGen prop "BF matches VSAT for a unbalanced choices of singletons"
  where
    prop :: ReadableProp
    prop = bnot $ bnot $ bChc "AA" (bChc "DD" (bRef "x") (bRef "y")) (bRef "z")

chc_balanced_unit = unitGen prop "BF matches VSAT for balanced choices"
  where prop :: ReadableProp
        prop = bChc "AA" (bRef "x") (bRef "y") &&& bChc "DD" (bRef "a") (bRef "b")

chc_2_nested_unit = unitGen prop "BF matches VSAT for 2 nested choices"
  where prop :: ReadableProp
        prop = bChc "AA"
          (bChc "BB" (bRef "x") (bRef "z"))
          (bChc "DD" false true)

bimpl_w_false_is_sat_unit = unitGen prop "BF matches VSAT for equivalency that is always unsat"
  where prop :: ReadableProp
        -- prop = ((bChc "AA" (bRef "a") (bRef "b")) ||| false) <=> false
        prop = (true ||| (bChc "AA" (bRef "a") (bRef "b"))) <=> false
        -- prop = (true <=> (bRef "a"))

-- | notice this fails because SBV adds extra unused variables into the model where BF doesn't
-- | TODO fix it by migrating away from SBV
bimpl_w_false_chc_is_sat_unit = unitGen prop "BF matches VSAT for equivalency that is always unsat with a choice"
  where prop :: ReadableProp
        prop = false <=> (bChc "AA" (bRef "a") (bRef "b"))

mixed_and_impl_is_sat_unit =
  unitGen prop "BF matches VSAT for equivalency that is always unsat"
  where prop :: ReadableProp
        prop = false &&& ((bChc "AA" (bRef "a") (bRef "b")) ==> (bRef "c"))

chces_not_in_model_unit = unitGen prop "BF never returns a model that contains a choice as a variable"
  where prop :: ReadableProp
        prop = (bChc "BB" (false) (bChc "CC" (bRef "a") (bRef "b"))) &&& true

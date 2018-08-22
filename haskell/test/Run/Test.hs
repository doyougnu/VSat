module Run.Test where

import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import Data.SBV ( SatResult(..)
                , SMTResult(..)
                , ThmResult(..)
                , SMTConfig(..)
                , SMTSolver(..)
                , Solver(..))
import Data.SBV.Internals (showModel, SMTModel(..))
import Control.Monad.Trans (liftIO)

import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen
import Config (defConf, allOptsConf)
import Run
import Api

instance Eq SMTResult where
  (Unsatisfiable x) == (Unsatisfiable y) = x == y
  (Satisfiable _ x) == (Satisfiable _ y) = x == y
  (SatExtField _ x) == (SatExtField _ y) = x == y
  (Unknown _ x)     == (Unknown _ y)     = x == y
  (ProofError _ x)  == (ProofError _ y)  = x == y
  _                 == _                 = False

instance Eq Solver where
  Z3        == Z3        = True
  Yices     == Yices     = True
  Boolector == Boolector = True
  CVC4      == CVC4      = True
  MathSAT   == MathSAT   = True
  ABC       == ABC       = True
  _         == _         = False

instance Eq SMTModel where
  m == n = (modelAssocs m) == (modelAssocs n)

instance Eq SMTConfig where
  (SMTConfig
   {verbose             = a
   ,timing              = _
   ,printBase           = c
   ,printRealPrec       = d
   ,satCmd              = e
   ,allSatMaxModelCount = f
   ,isNonModelVar       = _
   ,transcript          = h
   ,smtLibVersion       = _
   ,solver              = j
   ,roundingMode        = _
   ,solverSetOptions    = _
   ,ignoreExitCode      = _
   ,redirectVerbose     = n
   }) == (SMTConfig{..}) = a == verbose &&
                           c == printBase &&
                           d == printRealPrec &&
                           e == satCmd &&
                           (==) f allSatMaxModelCount &&
                           (==) h transcript &&
                           (name j) == (name solver) &&
                           (==) n redirectVerbose

instance Eq SatResult where (SatResult x) == (SatResult y) = x == y
instance Eq ThmResult where (ThmResult x) == (ThmResult y) = x == y

runProperties :: TestTree
runProperties = testGroup "Run Properties" [
  andDecomp_terminatesSh
  -- , sat_term
  -- sat_error
  -- , sat_error2
  -- , sat_error3
                                           -- ad_term2
                                           -- ad_term
                                           -- , qcProps
                                           ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
  sat_error
  , sat_error2
  , sat_error4
  , andDecomp_duplicate
  , andDecomp_duplicateChc
  ]

sat_term = QC.testProperty
           "Satisfiability terminates on any input"
           sat_terminates

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

andDecomp_duplicate = H.testCase
  "And decomposition can solve props with repeat variables" $
  do a <- runAD defConf prop
     H.assertBool "should never be empty" (not $ null a)
  where
    prop :: VProp String String
    prop = iRef "x" ./= iRef "x"

andDecomp_duplicateChc = H.testCase
  "And decomposition can solve props with repeat dimensions" $
  do a <- runAD defConf prop
     H.assertBool "should never be empty" (not $ null a)
  where
    prop :: VProp String String
    prop = ChcB "D" (bRef "c" &&& bRef "d") (bRef "a") &&& ChcB "D" (bRef "a") (bRef "c")

andDecomp_terminatesSh_ = QCM.monadicIO $
  do
    let gen = genVPropAtShare 5 $ vPropShare (repeat 4)
    prop <- QCM.run . QC.generate $ gen `QC.suchThat` onlyInts
    liftIO $ print "----\n"
    liftIO $ print prop
    liftIO $ print "----\n"
    a <- QCM.run $ runAD defConf (bimap show show prop)
    QCM.assert (not $ null a)

sat_terminates x =  onlyInts x QC.==> QCM.monadicIO
  $ do -- liftIO $ print $ "prop: " ++ show (x :: VProp Var Var) ++ " \n"
       a <- QCM.run . sat . bimap show show $ (x :: VProp Var Var)
       QCM.assert (not $ null a)

ad_terminates x = onlyInts x QC.==> QCM.monadicIO
  $ do -- liftIO $ print $ "prop: " ++ show (x :: VProp Var Var)
       -- liftIO $ print $ "prop Dup?: " ++ show (noDupRefs x)
       a <- QCM.run . runAD defConf . bimap show show $ (x :: VProp Var Var)
       QCM.assert (not $ null a)

sat_error_unit = do a <- sat prop
                    H.assertBool "" (not $ null a)
  where
    prop :: VProp String String
    prop = (signum 7 - (LitI . D $ 10.905)) ./=
           ((signum (signum (dRef "x"))) + signum 6)
    -- prop = (signum 7 - (LitI . D $ 10.905)) .== (0 :: VIExpr String)
    -- prop = (signum 7 - (LitI . D $ 10.905)) .== (0 :: VIExpr String)
    -- prop = ((dRef "x" - iRef "q") .== 0) &&& (bRef "w" &&& bRef "rhy")

sat_error_unit2 = do a <- sat prop
                     H.assertBool "" (not $ null a)
  where
    prop :: VProp String String
    prop = ((dRef "x") .<= (LitI . D $ 15.309)) &&& true
    -- prop = (dRef "x") .== (LitI . I $ 15) -- this passes

sat_error_unit3 = do a <- sat prop
                     H.assertBool "Modulus with Doubles passes as long as there is one integer" . not . null $ a
  where
    prop :: VProp String String
    prop = (dRef "x") .% (dRef "y") .> (LitI . I $ 1)

sat_error_unit4 = do a <- sat prop
                     H.assertBool "Division with a Double and Int coearces correctly" . not . null $ a
  where
    prop :: VProp String String
  -- this will still fail with a bitvec error
    prop =  (dRef "x") .% (-6) ./= (-(LitI . D $ 74.257))
    -- prop =  (abs (iRef "x")) .% (-6) ./= (-(LitI . D $ 74.257)) -- this is the original error, a define_fun
    -- prop = ((abs (dRef "x")) .% (-6) ./= (-(LitI . D $ 74.257))) <+> (bnot (bRef "y")) -- this throws a bitvec error
    -- (|ogzpzgeb| .% -6 ≠ -74.25731844390708) ⊻ ¬opvp

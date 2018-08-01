module Run.Test where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM
import Data.SBV ( SatResult(..)
                , SMTResult(..)
                , ThmResult(..)
                , SMTConfig(..)
                , SMTSolver(..)
                , Solver(..))
import Data.SBV.Internals (showModel, SMTModel(..))


import VProp.Types
import VProp.Core
import VProp.SBV
import VProp.Gen
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
  -- sat_term
                                           ad_term2
                                           -- , ad_term
                                           -- , qcProps
                                           ]

qcProps = QC.testProperty "and decomp is correct" $ \x -> andDecomp_correct x
ad_term = QC.testProperty
          "and decomp terminates on known failing example"
          andDecomp_terminates
ad_term2 = QC.testProperty
          "and decomp terminates on known failing example 2"
          andDecomp_terminates2

sat_term = QC.testProperty
           "Satisfiability terminates on any input"
           sat_terminates

andDecomp_correct x = not (null $ vars (x :: VProp Var Var)) QC.==> QCM.monadicIO $
  do a <- QCM.run $ runAD [] x'
     b <- QCM.run $ runAD [] x'
     assert ((head a) == (head b))
  where x' = bimap show show x

andDecomp_terminates = QCM.monadicIO $
  do a <- QCM.run $ runAD [] prop
     assert (not $ null a)
  where
    prop :: VProp String String
    prop = (dRef "a") .< (LitI . D $ 9.99999)


andDecomp_terminates2 = QCM.monadicIO $
  do a <- QCM.run $ runAD [] prop
     assert (not $ null a)
  where
    prop :: VProp String String
    prop = ((dRef "x" + iRef "q") .== 0) &&& true
    -- prop = ((dRef "x" - iRef "q") .== 0) &&& (bRef "w" &&& bRef "rhy")

andDecomp_terminates3 = QCM.monadicIO $
  do a <- QCM.run $ runAD [] prop
     assert (not $ null a)
  where
    prop :: VProp String String
    prop = ChcB "AA" (bRef "gd" &&& (iRef "j" .<= iRef "zy")) (false &&& bRef "g")
-- AA≺gd ∧ (j ≤ zy) , #F ∧ g≻

sat_terminates x = QCM.monadicIO $
  do a <- QCM.run . sat [] . bimap show show $ (x :: VProp Var Var)
     assert (not $ null a)

-- sat_error = QCM.monadicIO $
--   do a <- QCM.run $ runAD [] prop
--      assert (not $ null a)
--   where
--     prop :: VProp String String
--     prop =
--     -- prop = ((dRef "x" - iRef "q") .== 0) &&& (bRef "w" &&& bRef "rhy")

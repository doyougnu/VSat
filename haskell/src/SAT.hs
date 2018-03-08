module SAT where

import Control.Monad (liftM2)
import Data.SBV (Boolean(..),Predicate,SBool,Symbolic,isSatisfiable)
import System.IO.Unsafe (unsafePerformIO)


-- | A type class for types that can be converted to symbolic predicates
--   and checked by a SAT solver.
class Boolean b => SAT b where
  toPredicate :: b -> Predicate

-- | Is the predicate satisfiable?
satisfiable :: SAT b => b -> Bool
satisfiable b = unsafePerformIO (isSatisfiable (toPredicate b))

-- | Is the predicate unsatisfiable?
unsatisfiable :: SAT b => b -> Bool
unsatisfiable = not . satisfiable

-- | Is the predicate a tautology?
tautology :: SAT b => b -> Bool
tautology = unsatisfiable . bnot

-- | Are these predicates equivalent?
equivalent :: SAT b => b -> b -> Bool
equivalent a b = tautology (a <=> b)


-- Instances

instance Boolean b => Boolean (Symbolic b) where
  true  = return true
  false = return false
  bnot  = fmap bnot
  (&&&) = liftM2 (&&&)
  (|||) = liftM2 (|||)

instance SAT SBool where
  toPredicate = return

instance SAT Predicate where
  toPredicate = id

instance SAT (Symbolic Bool) where
  toPredicate = fmap fromBool

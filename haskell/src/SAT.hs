module SAT where

import           Control.Monad    (liftM2)
import           Data.SBV         (Predicate, SBool, Symbolic, sFalse, fromBool,
                                   isSatisfiable, sNot, sTrue, (.&&), (.<=>),
                                   (.||))
import           System.IO.Unsafe (unsafePerformIO)


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

class Boolean b where
  true  :: b
  false :: b
  bnot  :: b -> b
  (&&&) :: b -> b -> b
  (|||) :: b -> b -> b

  (<=>) :: b -> b -> b
  a <=> b = (a ==> b) &&& (b ==> a)

  (==>) :: b -> b -> b
  a ==> b = (bnot a) ||| b

  (<+>) :: b -> b -> b
  a <+> b = (a ||| b) &&& (bnot (a &&& b))

instance Boolean SBool where
  true  = sTrue
  false = sFalse
  bnot  = sNot
  (&&&) = (.&&)
  (|||) = (.||)
  (<=>) = (.<=>)
  {-# INLINE true #-}
  {-# INLINE false #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}
  {-# INLINE (<=>) #-}

instance Boolean Bool where
  true  =  True
  false = False
  bnot  = not
  (&&&) = (&&)
  (|||) = (||)
  {-# INLINE true  #-}
  {-# INLINE false #-}
  {-# INLINE bnot  #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}


instance Boolean b => Boolean (Symbolic b) where
  true  = return true
  false = return false
  bnot  = fmap bnot
  (&&&) = liftM2 (&&&)
  (|||) = liftM2 (|||)
  {-# INLINE true  #-}
  {-# INLINE false #-}
  {-# INLINE bnot  #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (|||) #-}

instance SAT SBool where
  toPredicate = return

instance SAT Predicate where
  toPredicate = id

instance SAT (Symbolic Bool) where
  toPredicate = fmap fromBool

module NVProp where

import           Data.Data       (Data, Typeable)
import           Data.String     (IsString)

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Data.SBV
import           SAT
import           GHC.Generics

-- | A feature is a named, boolean configuration option.
newtype Var = Var { featureName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable)

newtype Dim = Dim { dimName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,Mergable)

type FConfig b = Var -> b
type Config b = Dim -> Bool

--
-- * Syntax
--

-- | Boolean expressions over features.
data Prop
   = Lit Bool
   | Ref Var
   | Chc Dim Prop Prop
   | Not Prop
   | And Prop Prop
   | Or  Prop Prop
  deriving (Data,Eq,Typeable)

-- | The set of features referenced in a feature expression.
features :: Prop -> Set Var
features (Lit _)     = Set.empty
features (Ref f)     = Set.singleton f
features (Not e)     = features e
features (And l r)   = features l `Set.union` features r
features (Or  l r)   = features l `Set.union` features r
features (Chc _ l r) = features l `Set.union` features r

-- | The set of dimensions in a propositional expression
dimensions :: Prop -> Set Dim
dimensions (Lit _)     = Set.empty
dimensions (Ref _)     = Set.empty
dimensions (Not e)     = dimensions e
dimensions (And l r)   = dimensions l `Set.union` dimensions r
dimensions (Or  l r)   = dimensions l `Set.union` dimensions r
dimensions (Chc d l r) = Set.singleton d `Set.union`
                         dimensions l `Set.union` dimensions r

-- | Evaluate a feature expression against a configuration.
evalFeatureExpr :: Boolean b => Config b -> FConfig b -> Prop -> b
evalFeatureExpr _ _ (Lit b)   = if b then true else false
evalFeatureExpr _ c (Ref f)   = c f
evalFeatureExpr d c (Not e)   = bnot (evalFeatureExpr d c e)
evalFeatureExpr d c (And l r) = evalFeatureExpr d c l &&& evalFeatureExpr d c r
evalFeatureExpr d c (Or  l r) = evalFeatureExpr d c l ||| evalFeatureExpr d c r
evalFeatureExpr d c (Chc dim l r) = if d dim
                                    then evalFeatureExpr d c l
                                    else evalFeatureExpr d c r

-- | Pretty print a feature expression.
prettyFeatureExpr :: Prop -> String
prettyFeatureExpr = top
  where
    top (And l r)   = sub l ++ "∧" ++ sub r
    top (Or  l r)   = sub l ++ "∨" ++ sub r
    top (Chc d l r) = show d ++ "<" ++ sub l ++ " , " ++ sub r ++ " > "
    top e           = sub e
    sub (Lit b) = if b then "#T" else "#F"
    sub (Ref f) = featureName f
    sub (Not e) = "¬" ++ sub e
    sub e       = "(" ++ top e ++ ")"

-- | Generate a symbolic predicate for a feature expression.
symbolicFeatureExpr :: Prop -> Predicate
symbolicFeatureExpr e = do
    let fs = Set.toList (features e)
        ds = Set.toList (dimensions e)
    syms <- fmap (Map.fromList . zip fs) (sBools (map featureName fs))
    let dims = Map.fromList $ zip ds (map dimName ds)
    let look f = fromMaybe err (Map.lookup f syms)
    let lookd d = fromMaybe errd (Map.lookup d dims)
    return (evalFeatureExpr lookd look e)
  where err = error "symbolicFeatureExpr: Internal error, no symbol found."
        errd = error "symbolicFeatureExpr: Internal error, no dimension found."

-- | Reduce the size of a feature expression by applying some basic
--   simplification rules.
shrinkFeatureExpr :: Prop -> Prop
shrinkFeatureExpr e
    | unsatisfiable e           = Lit False
    | tautology e               = Lit True
shrinkFeatureExpr (Not (Not e)) = shrinkFeatureExpr e
shrinkFeatureExpr (And l r)
    | tautology l               = shrinkFeatureExpr r
    | tautology r               = shrinkFeatureExpr l
shrinkFeatureExpr (Or l r)
    | unsatisfiable l           = shrinkFeatureExpr r
    | unsatisfiable r           = shrinkFeatureExpr l
shrinkFeatureExpr e = e

instance Boolean Prop where
  true  = Lit True
  false = Lit False
  bnot  = Not
  (&&&) = And
  (|||) = Or

instance SAT Prop where
  toPredicate = symbolicFeatureExpr

instance Show Prop where
  show = prettyFeatureExpr

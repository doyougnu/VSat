module NVProp where

import           Data.Data       (Data, Typeable)
import           Data.String     (IsString)

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Data.SBV
import           GHC.Generics
import           SAT

-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable)

newtype Dim = Dim { dimName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable)

type FConfig b = Var -> b
type Config b = Dim -> b
type DimBool = Dim -> SBool

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
  deriving (Data,Eq,Generic,Typeable)


instance Mergeable Prop where
  symbolicMerge _ b thn els
    | Just result <- unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

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
evalPropExpr :: (Boolean b, Mergeable b) => DimBool -> FConfig b -> Prop -> b
evalPropExpr _ _ (Lit b)   = if b then true else false
evalPropExpr _ c (Ref f)   = c f
evalPropExpr d c (Not e)   = bnot (evalPropExpr d c e)
evalPropExpr d c (And l r) = evalPropExpr d c l &&& evalPropExpr d c r
evalPropExpr d c (Or  l r) = evalPropExpr d c l ||| evalPropExpr d c r
evalPropExpr d c (Chc dim l r) = ite (d dim)
                                    (evalPropExpr d c l)
                                    (evalPropExpr d c r)

-- | Pretty print a feature expression.
prettyPropExpr :: Prop -> String
prettyPropExpr = top
  where
    top (And l r)   = sub l ++ "∧" ++ sub r
    top (Or  l r)   = sub l ++ "∨" ++ sub r
    top (Chc d l r) = show d ++ "<" ++ sub l ++ ", " ++ sub r ++ ">"
    top e           = sub e
    sub (Lit b) = if b then "#T" else "#F"
    sub (Ref f) = varName f
    sub (Not e) = "¬" ++ sub e
    sub e       = "(" ++ top e ++ ")"

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: Prop -> Predicate
symbolicPropExpr e = do
    let fs = Set.toList (features e)
        ds = Set.toList (dimensions e)
    syms <- fmap (Map.fromList . zip fs) (sBools (map varName fs))
    dims <- fmap (Map.fromList . zip ds) (sBools (map dimName ds))
    let look f = fromMaybe err (Map.lookup f syms)
        lookd d = fromMaybe errd (Map.lookup d dims)
    return (evalPropExpr lookd look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."

-- | Reduce the size of a feature expression by applying some basic
--   simplification rules.
shrinkPropExpr :: Prop -> Prop
shrinkPropExpr e
    | unsatisfiable e           = Lit False
    | tautology e               = Lit True
shrinkPropExpr (Not (Not e)) = shrinkPropExpr e
shrinkPropExpr (And l r)
    | tautology l               = shrinkPropExpr r
    | tautology r               = shrinkPropExpr l
shrinkPropExpr (Or l r)
    | unsatisfiable l           = shrinkPropExpr r
    | unsatisfiable r           = shrinkPropExpr l
shrinkPropExpr e = e

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: Prop -> Prop
andDecomp (Chc d l r) = Or
                        (And (dimToVar d) (andDecomp l))
                        (And (Not (dimToVar d)) (andDecomp r))
  where dimToVar = Ref . Var . dimName
andDecomp (Not x)     = Not (andDecomp x)
andDecomp (Or l r)    = Or (andDecomp l) (andDecomp r)
andDecomp (And l r)   = And (andDecomp l) (andDecomp r)
andDecomp x           = x

instance Boolean Prop where
  true  = Lit True
  false = Lit False
  bnot  = Not
  (&&&) = And
  (|||) = Or

instance SAT Prop where
  toPredicate = symbolicPropExpr

instance Show Prop where
  show = prettyPropExpr

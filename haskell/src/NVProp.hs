module NVProp where

import           Data.Data       (Data, Typeable, typeOf)
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

type VConfig b = Var -> b
type DimBool = Dim -> SBool
type Config = Map.Map Dim Bool

--
-- * Syntax
--

-- | Boolean expressions over features.
data Prop
   = Lit Bool
   | Ref Var
   | Chc Dim Prop Prop
   | Not Prop
   | Op2 Op2 Prop Prop
  deriving (Data,Eq,Generic,Typeable)

data Op2 = And | Or | Impl | BiImpl deriving (Eq, Generic, Data, Typeable)

instance Mergeable Prop where
  symbolicMerge _ b thn els
    | Just result <- unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

----------------------------- Choice Manipulation ------------------------------
-- | Wrapper around engine
prune :: Prop -> Prop
prune = pruneTagTree Map.empty

-- | Given d config and variational expression remove redundant choices
pruneTagTree :: Config -> Prop -> Prop
pruneTagTree _ (Ref d) = Ref d
pruneTagTree _ (Lit b) = Lit b
pruneTagTree tb (Chc t y n) = case Map.lookup t tb of
                             Nothing -> Chc t
                                        (pruneTagTree (Map.insert t True tb) y)
                                        (pruneTagTree (Map.insert t False tb) n)
                             Just True -> pruneTagTree tb y
                             Just False -> pruneTagTree tb n
pruneTagTree tb (Not x)      = Not $ pruneTagTree tb x
pruneTagTree tb (Op2 a l r)  = Op2 a (pruneTagTree tb l) (pruneTagTree tb r)

-- | Given a config and a Variational Prop term select the element out that the
-- config points to
selectVariant :: Config -> Prop -> Maybe Prop
selectVariant _ (Ref a) = Just $ Ref a
selectVariant _ (Lit a) = Just $ Lit a
selectVariant tbs (Chc t y n) = case Map.lookup t tbs of
                           Nothing    -> Nothing
                           Just True  -> selectVariant tbs y
                           Just False -> selectVariant tbs n
selectVariant tb (Not x)      = Not   <$> selectVariant tb x
selectVariant tb (Op2 a l r)  = Op2 a <$> selectVariant tb l <*> selectVariant tb r

-- | Convert a dimension to a variable
dimToVar :: Dim -> Prop
dimToVar = Ref . Var . dimName

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: Prop -> Prop
andDecomp (Chc d l r) = (dimToVar d &&& andDecomp l) ||| (bnot (dimToVar d) &&& andDecomp r)
andDecomp (Not x)     = Not (andDecomp x)
andDecomp (Op2 c l r) = Op2 c (andDecomp l) (andDecomp r)
andDecomp x           = x

--------------------------- Getters --------------------------------------------
-- | The set of features referenced in a feature expression.
features :: Prop -> Set Var
features (Lit _)     = Set.empty
features (Ref f)     = Set.singleton f
features (Not e)     = features e
features (Op2 _ l r) = features l `Set.union` features r
features (Chc _ l r) = features l `Set.union` features r

-- | The set of dimensions in a propositional expression
dimensions :: Prop -> Set Dim
dimensions (Lit _)     = Set.empty
dimensions (Ref _)     = Set.empty
dimensions (Not e)     = dimensions e
dimensions (Op2 _ l r) = dimensions l `Set.union` dimensions r
dimensions (Chc d l r) = Set.singleton d `Set.union`
                         dimensions l `Set.union` dimensions r


------------------------------ Evaluation --------------------------------------
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: (Boolean b, Mergeable b) => DimBool -> VConfig b -> Prop -> b
evalPropExpr _ _ (Lit b)   = if b then true else false
evalPropExpr _ c (Ref f)   = c f
evalPropExpr d c (Not e)   = bnot (evalPropExpr d c e)
evalPropExpr d c (Op2 a l r)
  | typeOf a == typeOf And    = makePropWith (&&&)
  | typeOf a == typeOf Or     = makePropWith (|||)
  | typeOf a == typeOf Impl   = makePropWith (==>)
  | typeOf a == typeOf BiImpl = makePropWith (<=>)
    where makePropWith f = evalPropExpr d c l `f` evalPropExpr d c r
evalPropExpr d c (Chc dim l r) = ite (d dim)
                                    (evalPropExpr d c l)
                                    (evalPropExpr d c r)

-- | Pretty print a feature expression.
prettyPropExpr :: Prop -> String
prettyPropExpr = top
  where
    top (Op2 a l r)
      | typeOf a == typeOf And    = l' ++ "∧"  ++ r'
      | typeOf a == typeOf Or     = l' ++ "∨"  ++ r'
      | typeOf a == typeOf Impl   = l' ++ "->"  ++ r'
      | typeOf a == typeOf BiImpl = l' ++ "<->" ++ r'
      where l' = sub l
            r' = sub r

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
shrinkPropExpr (Op2 And l r)
    | tautology l               = shrinkPropExpr r
    | tautology r               = shrinkPropExpr l
shrinkPropExpr (Op2 Or l r)
    | unsatisfiable l           = shrinkPropExpr r
    | unsatisfiable r           = shrinkPropExpr l
shrinkPropExpr e = e

instance Boolean Prop where
  true  = Lit True
  false = Lit False
  bnot  = Not
  (&&&) = Op2 And
  (|||) = Op2 Or
  (==>) = Op2 Impl
  (<=>) = Op2 BiImpl

instance SAT Prop where
  toPredicate = symbolicPropExpr

instance Show Prop where
  show = prettyPropExpr

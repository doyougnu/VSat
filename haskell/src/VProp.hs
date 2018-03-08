module VProp where

import           Data.Data       (Data, Typeable, typeOf)
import           Data.String     (IsString)
import           Control.Monad   (liftM3, liftM2)

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Data.SBV
import           GHC.Generics
import           SAT

import Control.DeepSeq    (NFData)
import Test.QuickCheck    (Arbitrary, Gen, arbitrary, frequency, sized)
import Test.QuickCheck.Gen

-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary)

newtype Dim = Dim { dimName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary)

type VConfig b = Var -> b
type DimBool = Dim -> SBool
type Config = Map.Map Dim Bool

--
-- * Syntax
--

-- | Boolean expressions over features.
data VProp
   = Lit Bool
   | Ref Var
   | Chc Dim VProp VProp
   | Not VProp
   | Op2 Op2 VProp VProp
  deriving (Data,Eq,Generic,Typeable)

-- | data constructor for binary operations
data Op2 = And | Or | Impl | BiImpl deriving (Eq,Generic,Data,Typeable)

-- | smart constructors
_and :: VProp -> VProp -> VProp
_and = Op2 And

_or:: VProp -> VProp -> VProp
_or = Op2 Or

_impl:: VProp -> VProp -> VProp
_impl = Op2 Impl

_bimpl :: VProp -> VProp -> VProp
_bimpl = Op2 BiImpl

-- | Generate only alphabetical characters
genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

-- | generate a list of only alphabetical characters and convert to Dim
genAlphaNumStr :: Gen String
genAlphaNumStr = listOf genAlphaNum

genDim :: Gen Dim
genDim = Dim <$> genAlphaNumStr

genVar :: Gen Var
genVar = Var <$> genAlphaNumStr

-- | Generate an Arbitrary VProp, these frequencies can change for different
-- depths
arbVProp :: Int -> Gen VProp
arbVProp 0 = fmap Ref genVar
arbVProp n = frequency [ (1, fmap Ref genVar)
                       , (4, liftM3 Chc genDim l l)
                       , (3, fmap Not l)
                       , (3, liftM2 _or l l)
                       , (3, liftM2 _and l l)
                       , (3, liftM2 _impl l l)
                       , (3, liftM2 _bimpl l l)
                       ]
  where l = arbVProp (n `div` 2)

-- | Generate a random prop term according to arbVProp
genVProp :: IO VProp
genVProp = generate arbitrary
----------------------------- Choice Manipulation ------------------------------
-- | Wrapper around engine
prune :: VProp -> VProp
prune = pruneTagTree Map.empty

-- | Given d config and variational expression remove redundant choices
pruneTagTree :: Config -> VProp -> VProp
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

-- | Given a config and a Variational VProp term select the element out that the
-- config points to
selectVariant :: Config -> VProp -> Maybe VProp
selectVariant _ (Ref a) = Just $ Ref a
selectVariant _ (Lit a) = Just $ Lit a
selectVariant tbs (Chc t y n) = case Map.lookup t tbs of
                           Nothing    -> Nothing
                           Just True  -> selectVariant tbs y
                           Just False -> selectVariant tbs n
selectVariant tb (Not x)      = Not   <$> selectVariant tb x
selectVariant tb (Op2 a l r)  = Op2 a <$> selectVariant tb l <*> selectVariant tb r

-- | Convert a dimension to a variable
dimToVar :: Dim -> VProp
dimToVar = Ref . Var . dimName

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: VProp -> VProp
andDecomp (Chc d l r) = (dimToVar d &&& andDecomp l) |||
                        (bnot (dimToVar d) &&& andDecomp r)
andDecomp (Not x)     = Not (andDecomp x)
andDecomp (Op2 c l r) = Op2 c (andDecomp l) (andDecomp r)
andDecomp x           = x

--------------------------- Getters --------------------------------------------
-- | The set of features referenced in a feature expression.
features :: VProp -> Set Var
features (Lit _)     = Set.empty
features (Ref f)     = Set.singleton f
features (Not e)     = features e
features (Op2 _ l r) = features l `Set.union` features r
features (Chc _ l r) = features l `Set.union` features r

-- | The set of dimensions in a propositional expression
dimensions :: VProp -> Set Dim
dimensions (Lit _)     = Set.empty
dimensions (Ref _)     = Set.empty
dimensions (Not e)     = dimensions e
dimensions (Op2 _ l r) = dimensions l `Set.union` dimensions r
dimensions (Chc d l r) = Set.singleton d `Set.union`
                         dimensions l `Set.union` dimensions r


------------------------------ Evaluation --------------------------------------
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: (Boolean b, Mergeable b) => DimBool -> VConfig b -> VProp -> b
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
prettyPropExpr :: VProp -> String
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
symbolicPropExpr :: VProp -> Predicate
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
shrinkPropExpr :: VProp -> VProp
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

instance Boolean VProp where
  true  = Lit True
  false = Lit False
  bnot  = Not
  (&&&) = Op2 And
  (|||) = Op2 Or
  (==>) = Op2 Impl
  (<=>) = Op2 BiImpl

instance SAT VProp where
  toPredicate = symbolicPropExpr

instance Show VProp where
  show = prettyPropExpr

-- | make prop mergeable so choices can use symbolic conditionals
instance Mergeable VProp where
  symbolicMerge _ b thn els
    | Just result <- unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

-- | arbritrary instance for the generator monad
instance Arbitrary VProp where
  arbitrary = sized arbVProp

-- | Deep Seq instances for Criterion Benchmarking
instance NFData VProp
instance NFData Op2

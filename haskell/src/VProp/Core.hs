module VProp.Core where

import           Control.Monad (liftM2)
import           Data.List     (group, sort, nub)
import qualified Data.Map      as Map
import           Data.Monoid   ((<>), Sum(..))
import           Data.SBV      (literal)
import qualified Data.Set      as Set
import           Data.Text     (unpack)
import           Prelude       hiding (EQ, GT, LT)

import           Utils
import           VProp.Types

instance Show Var where show = unpack . varName
instance (Show a, Show b, Show c) => Show (VProp a b c) where
  show = prettyPropExpr


-- | Pretty print a feature expression.
instance Show NPrim where show (I i) = show i
                          show (D d) = show d

instance Show NN_N where show Add  = "+"
                         show Sub  = "-"
                         show Mult = "*"
                         show Div  = "/"
                         show Mod  = "%"

instance Show NN_B where show LT  = "<"
                         show LTE = "≤"
                         show GT  = ">"
                         show GTE = "≥"
                         show EQ  = "=="
                         show NEQ = "≠"

instance Show BB_B where show Impl   = "→"
                         show BiImpl = "↔"
                         show XOr    = "⊻"
                         show And    = "∧"
                         show Or     = "∨"

instance (Show a, Show b) => Show (VIExpr a b) where
  show (LitI a) = show a
  show (Ref _ a) = show a
  show (OpI Neg a) = "¬" <> "(" <> show a <> ")"
  show (OpI Abs a) = "|" <> show a <> "|"
  show (OpI Sign a) = "signum " <> show a
  show (OpII f l r) = mconcat [show l, " ", show f, " ", show r]
  show (ChcI d l r) = mconcat [show $ dimName d, "≺", show l, ", ", show r, "≻"]

instance Show B_B where show Not = "¬"

prettyPropExpr :: (Show a, Show b, Show c) => VProp a b c -> String
prettyPropExpr = top
  where
    top :: (Show a, Show b, Show c) => VProp a b c -> String
    top !(OpBB b l r)    = mconcat [sub l, " ", show b, " ", sub r]
    top !(OpIB nb l r)   = mconcat [show l, " ", show nb, " ", show r]
    top !(ChcB d ls rs)  = show (dimName d) <> "≺" <> top ls <> ", " <> top rs <> "≻"
    top !e               = sub e

    sub :: (Show a, Show b, Show c) => VProp a b c -> String
    sub !(LitB b)  = if b then "#T" else "#F"
    sub !(RefB f)  = show f
    sub !(OpB b e) = show b <> sub e
    sub !e         = "(" <> top e <> ")"

----------------------------- Conversion --------------------------------------
iToSNum :: Integer -> SNum
iToSNum = SI . literal . fromIntegral

dToSNum :: Double -> SNum
dToSNum = SD . literal

----------------------------- Predicates ---------------------------------------
-- | true iff a propositions has no chcs whatsoever
isPlain :: VProp a b c -> Bool
isPlain = null . trifoldMap (:[]) mempty mempty

-- | Does the prop contain choices
hasChc :: VProp a b c -> Bool
hasChc = not . isPlain

-- | Does the prop only contain boolean values? No ints or floats
onlyBools :: VProp d a b -> Bool
onlyBools (OpIB _ _ _ ) = False
onlyBools (ChcB _ l r)  = onlyBools l && onlyBools r
onlyBools (OpBB _ l r)  = onlyBools l && onlyBools r
onlyBools (OpB  _ e)    = onlyBools e
onlyBools _             = True

-- | true iff the prop only contains references and literals that are integers
-- no doubles. This is mainly used for random generators
onlyInts :: VProp d a b -> Bool
onlyInts (OpIB _ l r) = onlyInts' l && onlyInts' r
onlyInts (ChcB _ l r) = onlyInts l && onlyInts r
onlyInts (OpBB _ l r) = onlyInts l && onlyInts r
onlyInts (OpB _ e)    = onlyInts e
onlyInts _            = True

onlyInts' :: VIExpr d b -> Bool
onlyInts' (LitI (D _)) = False
onlyInts' (Ref RefD _) = False
onlyInts' (OpI _ e)    = onlyInts' e
onlyInts' (OpII _ l r) = onlyInts' l && onlyInts' r
onlyInts' (ChcI _ l r) = onlyInts' l && onlyInts' r
onlyInts' _            = True

-- TODO fix this double traversal
-- | true iff the prop only contains literals, no variable references
onlyLits :: VProp d a b -> Bool
onlyLits p = (null $ trifoldMap mempty (:[]) mempty p) &&
             (null $ trifoldMap mempty mempty (:[]) p)

-- | Are there any variables in the boolean language that shadow variables in
-- the integer language?
noDupRefs :: Ord a => VProp d a a -> Bool
noDupRefs prop = Set.null $ (bvars prop) `Set.intersection` (ivars prop)

-- ----------------------------- Choice Manipulation ------------------------------
-- | Given a config and a Variational VProp term select the element out that the
-- config points to
selectVariant :: Ord d => Config d -> VProp d a b -> Maybe (VProp d a b)
selectVariant tbs x@(ChcB t y n) = case Map.lookup t tbs of
                                     Nothing    -> Just x
                                     Just True  -> selectVariant tbs y
                                     Just False -> selectVariant tbs n
selectVariant tb (OpB op x)    = OpB op <$> selectVariant tb x
selectVariant tb (OpBB a l r)  = liftM2 (OpBB a)
                                (selectVariant tb l)
                                (selectVariant tb r)
selectVariant tb (OpIB op l r) = OpIB op <$>
                                 selectVariant' tb l <*>
                                 selectVariant' tb r
selectVariant _  x             = Just x

selectVariant' :: Ord d => Config d -> VIExpr d b -> Maybe (VIExpr d b)
selectVariant' tb x@(ChcI t y n) = case Map.lookup t tb of
                                     Nothing    -> Just x
                                     Just True  -> selectVariant' tb y
                                     Just False -> selectVariant' tb n
selectVariant' tb (OpI op e)    = OpI op <$> selectVariant' tb e
selectVariant' tb (OpII op l r) = liftM2 (OpII op) (selectVariant' tb l) (selectVariant' tb r)
selectVariant' _  x             = Just x


-- | Convert a dimension to a variable
dimToVarBy :: (Dim d -> a) -> Dim d -> (VProp d a b)
dimToVarBy f = RefB . f

dimToVar :: Dim d -> (VProp d d b)
dimToVar = RefB . dimName

-- --------------------------- Descriptors ----------------------------------------
-- | TODO fix all this redundancy by abstracting the dimensions and instancing Bifoldable
-- | Convert a prop into a list of Terms
toList :: VProp d a b -> [VProp d a b]
toList prop = go prop []
  where
    go :: VProp d a b -> [VProp d a b] -> [VProp d a b]
    go x@(OpB _ a) acc    = go a $ x:acc
    go x@(ChcB _ l r) acc = go r . go l $ x:acc
    go x@(OpBB _ l r) acc = go r . go l $ x:acc
    go a acc              = a:acc

numTerms :: VProp d a b -> Integer
numTerms = getSum . trifoldMap f f f
  where f = const 1


numVars :: VProp d a b -> Integer
numVars = getSum . trifoldMap (const 0) f f
  where f = const 1

-- | Count the choices in a tree
numChc :: VProp d a b -> Integer
numChc = toInteger . length . trifoldMap (:[]) mempty mempty

-- | Count the plain values in a tree
numPlain :: VProp d a b -> Integer
numPlain = toInteger . length . filter isPlain . toList

-- | Given a vprop how many shared dimensions were there
numSharedDims :: (Eq d) => VProp d a b -> Integer
numSharedDims = toInteger . length . filter (flip (>=) 2 . length) . group . trifoldMap (:[]) mempty mempty

numDims :: VProp d a b -> Integer
numDims = getSum . trifoldMap (const 1) mempty mempty

-- | Number of like plain terms
numSharedPlain :: (Eq d, Eq a, Eq b) => VProp d a b -> Integer
numSharedPlain = toInteger . length . filter (flip (>=) 2 . length) . group . filter isPlain . toList

-- | Given a prop return the maximum number of times a given dimension was shared
maxShared :: (Eq d, Ord d) => VProp d a b -> Int
maxShared = safeMaximum . fmap length . group . sort . trifoldMap (:[]) mempty mempty
  where safeMaximum [] = 0
        safeMaximum xs = maximum xs

configToProp :: Config a -> VProp a a a
configToProp = Map.foldrWithKey' step true
  where step :: Dim a -> Bool -> VProp a a a -> VProp a a a
        step d True  acc = OpBB And (RefB $ dimName d) acc
        step d False acc = OpBB And (bnot . RefB $ dimName d) acc

--------------------------- Destructors -----------------------------------------
-- | The set of features referenced in a feature expression.
bvars :: Ord a => (VProp d a b) -> Set.Set a
bvars = trifoldMap mempty Set.singleton mempty

-- | The set of dimensions in a propositional expression
dimensions :: Ord d => (VProp d a b) -> Set.Set (Dim d)
dimensions = trifoldMap (Set.singleton . Dim) mempty mempty

dimensions' :: Eq d => VProp d a b -> [Dim d]
dimensions' = nub . trifoldMap ((:[]) . Dim) mempty mempty

-- | wrapper around choices engine, called choices_ with [] results in [] always
-- being returned. Works properly with [[]]
choices :: Eq d => VProp d a b -> [[(Dim d, Bool)]]
choices = booleanCombinations . dimensions'

-- | TODO implement choices' for IBs
choices' :: (Ord b) => VIExpr d b -> Set.Set (RefN, b)
choices' = undefined

-- | The set of integar variable references for an expression
ivars :: (Ord b) => VProp d a b -> Set.Set b
ivars = trifoldMap mempty mempty Set.singleton

-- | The set of integar variable references for an expression
-- we save the leading constructors i.e. RefI or RefD so we know whether to call
-- sInteger or sDouble in evalPropExpr
ivarsWithType :: (Ord b) => VProp d a b -> Set.Set (RefN, b)
ivarsWithType (LitB _)     = Set.empty
ivarsWithType (RefB _)     = Set.empty
ivarsWithType (OpB _ e)    = ivarsWithType e
ivarsWithType (OpBB _ l r) = ivarsWithType l `Set.union` ivarsWithType r
ivarsWithType (OpIB _ l r) = ivarsWithType' l `Set.union` ivarsWithType' r
ivarsWithType (ChcB _ l r) = ivarsWithType l `Set.union` ivarsWithType r

ivarsWithType' :: (Ord b) => VIExpr d b -> Set.Set (RefN, b)
ivarsWithType' (LitI _)     = Set.empty
ivarsWithType' (OpI _ e)    = ivarsWithType' e
ivarsWithType' (OpII _ l r) = ivarsWithType' l `Set.union` ivarsWithType' r
ivarsWithType' (ChcI _ l r) = ivarsWithType' l `Set.union` ivarsWithType' r
ivarsWithType' (Ref x a)    = Set.singleton (x, a)

-- | The set of boolean variable references for an expression
vars :: (Ord a) => VProp d a a -> Set.Set a
vars = trifoldMap mempty Set.singleton Set.singleton


--------------------------- Constructors -----------------------------------------
conjoin :: [VProp d a b] -> VProp d a b
conjoin = fromList $ OpBB And

disjoin :: [VProp d a b] -> VProp d a b
disjoin = fromList $ OpBB Or

xOrJoin :: [VProp d a b] -> VProp d a b
xOrJoin = fromList $ OpBB XOr

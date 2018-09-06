module VProp.Core where

import           Control.Monad       (liftM, liftM2)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.List           (intercalate,group,sort)
import           Data.Monoid         ((<>))
import           Prelude hiding      (LT, GT, EQ)


import VProp.Types

instance Show Var where show = varName
instance (Show a, Show b) => Show (VProp a b c) where show = prettyPropExpr

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

instance (Show a, Show b) => Show (VIExpr a b) where
  show (LitI a) = show a
  show (Ref _ a) = show a
  show (OpI Neg a) = "¬" <> "(" <> show a <> ")"
  show (OpI Abs a) = "|" <> show a <> "|"
  show (OpI Sign a) = "signum " <> show a
  show (OpII f l r) = mconcat [show l, " ", show f, " ", show r]
  show (ChcI d l r) = mconcat [show d, "≺", show l, ", ", show r, "≻"]

instance Show B_B where show Not = "¬"

prettyPropExpr :: (Show a, Show b, Show c) => VProp a b c -> String
prettyPropExpr = top
  where
    top :: (Show a, Show b) => VProp a b c -> String
    top (Opn Or ps)     = intercalate " ∨ " $ sub <$> ps
    top (Opn And ps)    = intercalate " ∧ " $ sub <$> ps
    top (OpBB b l r)    = mconcat [sub l, " ", show b, " ", sub r]
    top (OpIB nb l r)    = mconcat [show l, " ", show nb, " ", show r]
    top (ChcB d ls rs) = show d ++ "≺" ++ top ls ++ ", " ++ top rs++ "≻"
    top e           = sub e

    sub :: (Show a, Show b, Show c) => VProp a b c -> String
    sub (LitB b) = if b then "#T" else "#F"
    sub (RefB f) = show f
    sub (OpB b e) = show b <> sub e
    sub e       = "(" ++ top e ++ ")"

----------------------------- Predicates ---------------------------------------
-- | true if a propositions has no chcs whatsoever
isPlain :: VProp a b c -> Bool
isPlain (ChcB _ _ _) = False
isPlain (OpB _ x)     = isPlain x
isPlain (Opn _ ps)  = all isPlain ps
isPlain (OpBB _ l r) = isPlain l && isPlain r
isPlain (OpIB _ l r) = isPlain' l && isPlain' r
isPlain _           = True

isPlain' :: VIExpr a b -> Bool
isPlain' (ChcI _ _ _) = False
isPlain' (OpII _ l r) = isPlain' l && isPlain' r
isPlain' (OpI _ e)    = isPlain' e
isPlain' _            = True

-- | Does the prop contain choices
isChc :: VProp a b c -> Bool
isChc = not . isPlain

-- | Does the prop only contain boolean values? No ints or floats
onlyBools :: VProp a b c -> Bool
onlyBools (OpIB _ _ _ ) = False
onlyBools (ChcB _ l r)  = onlyBools l && onlyBools r
onlyBools (Opn _ xs)    = foldr (\x acc -> acc && onlyBools x) True xs
onlyBools (OpBB _ l r)  = onlyBools l && onlyBools r
onlyBools (OpB  _ e)    = onlyBools e
onlyBools _             = True

-- | Does the prop only contain Ints
onlyInts :: VProp a b c -> Bool
onlyInts (OpIB _ l r) = onlyInts' l && onlyInts' r
onlyInts (ChcB _ l r) = onlyInts l && onlyInts r
onlyInts (Opn _ xs)   = foldr (\x acc -> acc && onlyInts x) True xs
onlyInts (OpBB _ l r) = onlyInts l && onlyInts r
onlyInts (OpB _ e)    = onlyInts e
onlyInts _            = True

onlyInts' :: VIExpr a b -> Bool
onlyInts' (LitI (D _)) = False
onlyInts' (OpI _ e)    = onlyInts' e
onlyInts' (OpII _ l r) = onlyInts' l && onlyInts' r
onlyInts' (ChcI _ l r) = onlyInts' l && onlyInts' r
onlyInts' _            = True

-- | Does the prop contain no variables?
onlyLits :: VProp a b c -> Bool
onlyLits (LitB _) = True
onlyLits (RefB _) = False
onlyLits (OpIB _ l r) = onlyLits' l && onlyLits' r
onlyLits (ChcB _ l r) = onlyLits l && onlyLits r
onlyLits (Opn _ xs)   = foldr (\x acc -> acc && onlyLits x) True xs
onlyLits (OpBB _ l r) =  onlyLits l && onlyLits r
onlyLits (OpB _ e)    = onlyLits e

onlyLits' :: VIExpr a b -> Bool
onlyLits' (LitI _)  = True
onlyLits' (Ref _ _) = False
onlyLits' (OpI _ e) = onlyLits' e
onlyLits' (OpII _ l r) = onlyLits' l && onlyLits' r
onlyLits' (ChcI _ l r) = onlyLits' l && onlyLits' r

-- | Are there any variables in the boolean language that shadow variables in
-- the integer language?
noDupRefs :: Ord b => VProp a b c -> Bool
noDupRefs prop = Set.null $ (bvars prop) `Set.intersection` (ivars prop)

-- ----------------------------- Choice Manipulation ------------------------------
-- | Given a config and a Variational VProp term select the element out that the
-- config points to
selectVariant :: Ord a => Config a -> VProp a b c -> Maybe (VProp a b c)
selectVariant tbs x@(ChcB t y n) = case Map.lookup t tbs of
                                     Nothing    -> Just x
                                     Just True  -> selectVariant tbs y
                                     Just False -> selectVariant tbs n
selectVariant tb (OpB op x)    = OpB op <$> selectVariant tb x
selectVariant tb (Opn a ps)    = liftM (Opn a) (sequence $ selectVariant tb <$> ps)
selectVariant tb (OpBB a l r)  = liftM2 (OpBB a)
                                (selectVariant tb l)
                                (selectVariant tb r)
selectVariant tb (OpIB op l r) = OpIB op <$> selectVariant' tb l <*> selectVariant' tb r
selectVariant _  x             = Just x

selectVariant' :: Ord a => Config a -> VIExpr a b -> Maybe (VIExpr a b)
selectVariant' tb x@(ChcI t y n) = case Map.lookup t tb of
                                     Nothing    -> Just x
                                     Just True  -> selectVariant' tb y
                                     Just False -> selectVariant' tb n
selectVariant' tb (OpI op e)    = OpI op <$> selectVariant' tb e
selectVariant' tb (OpII op l r) = liftM2 (OpII op) (selectVariant' tb l) (selectVariant' tb r)
selectVariant' _  x             = Just x


-- | Convert a dimension to a variable
dimToBvar :: Show a => (a -> b) -> a -> VProp a b c
dimToBvar f = RefB . f

dimToIvar :: Show a => (a -> c) -> a -> VProp a b c
dimToIvar f = RefB . f

-- --------------------------- Descriptors ----------------------------------------
-- | TODO fix all this redundancy by abstracting the dimensions and instancing Bifoldable
-- | Convert a prop into a list of Terms
toList :: VProp a b c -> [VProp a b c]
toList prop = go prop []
  where
    go :: VProp a b c -> [VProp a b c] -> [VProp a b c]
    go x@(OpB _ a) acc    = go a $ x:acc
    go x@(Opn _ ps) acc   = foldr go (x:acc) ps
    go x@(ChcB _ l r) acc = go r . go l $ x:acc
    go x@(OpBB _ l r) acc = go r . go l $ x:acc
    go a acc = a:acc

numTerms :: VProp a b c -> Integer
numTerms = toInteger. length . toList

-- | Count the choices in a tree
numChc :: VProp a b c -> Integer
numChc = toInteger . length . filter isChc . toList

-- | Count the plain values in a tree
numPlain :: VProp a b c -> Integer
numPlain = toInteger . length . filter isPlain . toList

-- | Given a vprop how many shared dimensions were there
numSharedDims :: (Eq a, Eq b, Eq c) => VProp a b c -> Integer
numSharedDims = toInteger . length . filter (flip (>=) 2 . length) . group . flip go []
  where
    go :: VProp a b c -> [a] -> [a]
    go (OpB _ a) acc    = go a acc
    go (OpIB _ l r) acc = go' l (go' r acc)
    go (OpBB _ l r) acc = go l (go r acc)
    go (Opn _ ps) acc   = foldr go acc ps
    go (ChcB d l r) acc = go l (go r $ d:acc)
    go _    acc         = acc

    go' :: VIExpr a b -> [a] -> [a]
    go' (ChcI d l r) acc = go' l (go' r $ d:acc)
    go' (OpII _ l r) acc = go' l (go' r acc)
    go' (OpI  _ e)   acc = go' e acc
    go' _            acc = acc

numSharedPlain :: (Eq a, Eq b) => VProp a b c -> Integer
numSharedPlain = toInteger . length . filter (flip (>=) 2 . length) . group . filter isPlain . toList

-- -- | Depth of the Term tree
-- depth :: (VProp a) -> Integer
-- depth prop = go prop 0
--   where
--     go :: (VProp a) -> Integer -> Integer
--     go (Not a) acc     = go a (succ acc)
--     go (Op2 _ l r) acc = max (go l (succ acc)) (go r (succ acc))
--     go (Opn _ ps)  acc = maximum $ flip go acc <$> ps
--     go (Chc _ l r) acc = max (go l acc) (go r acc)
--     go _ acc           = acc

-- | Given a prop return the maximum number of times a given dimension was shared
maxShared :: Show a => VProp a b c -> Int
maxShared = safeMaximum . fmap length . group . sort . go
  where go :: Show a =>  VProp a b c -> [String]
        go (ChcB d l r) = (show d) : go l ++ go r
        go (OpB _ l)     = go l
        go (Opn _ ps)  = foldMap go ps
        go (OpBB _ l r) = go l ++ go r
        go (OpIB _ l r) = go' l ++ go' r
        go _           = []

        go' :: Show a => VIExpr a b -> [String]
        go' (ChcI d l r) = (show d) : go' l ++ go' r
        go' (OpI _ l)    = go' l
        go' (OpII _ l r) = go' l ++ go' r
        go' _            = []

        safeMaximum [] = 0
        safeMaximum xs = maximum xs

-- --------------------------- Destructors -----------------------------------------
-- | The set of features referenced in a feature expression.
vars :: Ord b => (VProp a b b) -> Set.Set b
vars (LitB _)     = Set.empty
vars (RefB f)     = Set.singleton f
vars (OpB _ e)    = vars e
vars (OpBB _ l r) = vars l `Set.union` vars r
vars (OpIB _ _ _) = Set.empty
vars (Opn _ ps)   = Set.unions $ vars <$> ps
vars (ChcB _ l r) = vars l `Set.union` vars r

vars' :: Ord b => VIExpr a b -> Set.Set b
vars' (LitI _) = Set.empty
vars' (Ref _ f) = Set.singleton f
vars' (OpI _ e) = vars' e
vars' (OpII _ l r) = vars' l `Set.union` vars' r
vars' (ChcI _ l r) = vars' l `Set.union` vars' r

-- | The set of dimensions in a propositional expression
dimensions :: Ord a => VProp a b c -> Set.Set a
dimensions (LitB _)     = Set.empty
dimensions (RefB _)     = Set.empty
dimensions (OpB _ e)    = dimensions e
dimensions (OpBB _ l r) = dimensions l `Set.union` dimensions r
dimensions (OpIB _ l r) = dimensions' l `Set.union` dimensions' r
dimensions (Opn _ ps)   = Set.unions $ dimensions <$> ps
dimensions (ChcB d l r) = Set.singleton d `Set.union`
                            dimensions l `Set.union` dimensions r

dimensions' :: Ord a => VIExpr a b -> Set.Set a
dimensions' (LitI _)     = Set.empty
dimensions' (Ref _ _)     = Set.empty
dimensions' (OpI _ e)    = dimensions' e
dimensions' (OpII _ l r) = dimensions' l `Set.union` dimensions' r
dimensions' (ChcI d l r) = Set.singleton d `Set.union`
                             dimensions' l `Set.union` dimensions' r

-- | The set of integar variable references for an expression
ivars :: Ord b => VProp a b c -> Set.Set b
ivars (LitB _)     = Set.empty
ivars (RefB _)     = Set.empty
ivars (OpB _ e)    = ivars e
ivars (OpBB _ l r) = ivars l `Set.union` ivars r
ivars (OpIB _ l r) = ivars' l `Set.union` ivars' r
ivars (Opn _ ps)   = Set.unions $ ivars <$> ps
ivars (ChcB _ l r) = ivars l `Set.union` ivars r

ivars' :: Ord b => VIExpr a b -> Set.Set b
ivars' (LitI _)     = Set.empty
ivars' (OpI _ e)    = ivars' e
ivars' (OpII _ l r) = ivars' l `Set.union` ivars' r
ivars' (ChcI _ l r) = ivars' l `Set.union` ivars' r
ivars' (Ref _ a)    = Set.singleton a

-- | The set of integar variable references for an expression
-- we save the leading constructors i.e. RefI or RefD so we know whether to call
-- sInteger or sDouble in evalPropExpr
ivarsWithType :: Ord c => VProp a b c -> Set.Set (RefN, c)
ivarsWithType (LitB _)     = Set.empty
ivarsWithType (RefB _)     = Set.empty
ivarsWithType (OpB _ e)    = ivarsWithType e
ivarsWithType (OpBB _ l r) = ivarsWithType l `Set.union` ivarsWithType r
ivarsWithType (OpIB _ l r) = ivarsWithType' l `Set.union` ivarsWithType' r
ivarsWithType (Opn _ ps)   = Set.unions $ ivarsWithType <$> ps
ivarsWithType (ChcB _ l r) = ivarsWithType l `Set.union` ivarsWithType r

ivarsWithType' :: (Ord a, Ord b) => VIExpr a b -> Set.Set (RefN, b)
ivarsWithType' (LitI _)     = Set.empty
ivarsWithType' (OpI _ e)    = ivarsWithType' e
ivarsWithType' (OpII _ l r) = ivarsWithType' l `Set.union` ivarsWithType' r
ivarsWithType' (ChcI _ l r) = ivarsWithType' l `Set.union` ivarsWithType' r
ivarsWithType' (Ref x a)    = Set.singleton (x, a)

-- | The set of boolean variable references for an expression
bvars :: Ord b => VProp a b c -> (b -> c) Set.Set b
bvars prop = vars prop `Set.difference` ivars prop

-- | The set of all choices
configs :: Ord a => VProp a b c -> [[(a, Bool)]]
configs prop = go (Set.toList $ dimensions prop)
  where
    go []     = [[]]
    go (d:ds) = fmap ((d, True) :) cs ++ fmap ((d, False) :) cs
          where cs = go ds


-- -- | Given a Variational Prop term, get all possible paths in the choice tree
-- paths :: VProp a -> Set.Set Config
-- paths = Set.fromList . filter (not . Map.null) . go
--   where go (Chc d l r) = do someL <- go l
--                             someR <- go r
--                             [Map.insert d True someL, Map.insert d False someR]
--         go (Not x)     = go x
--         go (Op2 _ l r) = go l <> go r
--         go (Opn _ ps)  = concatMap go $ ps
--         go _           = [Map.empty]

-- ------------------------------ Manipulation ------------------------------------
-- -- | Given a tag tree, fmap over the tree with respect to a config
-- replace :: Config -> a -> VProp a -> VProp a
-- replace _    v (Ref _) = Ref v
-- replace conf v (Chc d l r) =
--   case Map.lookup d conf of
--     Nothing    -> Chc d (replace conf v l) (replace conf v r)
--     Just True  -> Chc d (replace conf v l) r
--     Just False -> Chc d l (replace conf v r)
-- replace conf v (Not x) = Not $ replace conf v x
-- replace conf v (Op2 a l r) = Op2 a (replace conf v l) (replace conf v r)
-- replace conf v (Opn a ps)  = Opn a (replace conf v <$> ps)
-- replace _    _ x           = x

-- -- | Given a Vprop term with an associated list of elements with their
-- -- configurations, replace each dimension with its associated value
-- recompile :: VProp a -> [(Config, a)] -> VProp a
-- recompile = foldr' (\(conf, val) acc -> replace conf val acc)

module VProp where

import           Control.Monad       (liftM, liftM2, liftM3)
import           Data.Data           (Data, Typeable)
import           Data.Foldable       (foldr', foldr1)
import           Data.List           (intercalate, group, sort)
import           Data.Monoid         ((<>))
import           Data.String         (IsString)

import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           GHC.Generics
import           SAT
import           Data.SBV

import           Control.DeepSeq     (NFData)
import           Data.Char           (toUpper)
import           Test.QuickCheck     (Arbitrary, Gen, arbitrary, frequency,
                                      sized)
import           Test.QuickCheck.Gen

-- | A feature is a named, boolean configuration option.
newtype Var = Var { varName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary)

newtype Dim = Dim { dimName :: String }
  deriving (Data,Eq,IsString,Ord,Show,Typeable,Generic,NFData,Arbitrary)

type VConfig a b = a -> b
type DimBool = Dim -> SBool
type Config = Map.Map Dim Bool

--
-- * Syntax
--

-- | Boolean expressions over features.
data VProp a
   = Lit Bool
   | Ref a
   | Chc Dim (VProp a) (VProp a)
   | Not (VProp a)
   | Opn Opn [(VProp a)]
   | Op2 Op2 (VProp a) (VProp a)
  deriving (Data,Eq,Generic,Typeable,Functor,Traversable,Foldable)

-- | data constructor for binary operations
data Op2 = Impl | BiImpl deriving (Eq,Generic,Data,Typeable, Show)
data Opn = And | Or deriving (Eq,Generic,Data,Typeable, Show)

----------------------------- Generators ---------------------------------------
-- | Generate only alphabetical characters
genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

-- | generate a list of only alphabetical characters and convert to Dim
genAlphaNumStr :: Gen String
genAlphaNumStr = flip suchThat (not . null) $ listOf genAlphaNum

genDim :: Gen Dim
genDim = Dim <$> (fmap . fmap) toUpper genAlphaNumStr

genSharedDim :: Gen Dim
genSharedDim = elements $
  zipWith  (\a b -> Dim $ toUpper <$> [a, b]) ['a'..'f'] ['a'..'f']

genVar :: Gen String
genVar = genAlphaNumStr

newtype Readable = Re { readStr :: String }
instance Show Readable where
  show = show . readStr

instance Arbitrary Readable where
  arbitrary = Re <$> genAlphaNumStr

-- | Generate an Arbitrary VProp, given a generator and counter these
-- frequencies can change for different depths. The counter is merely for a
-- `sized` call
arbVProp :: Arbitrary a => Int -> Gen Dim -> Gen (VProp a)
arbVProp 0 _    = Ref <$> arbitrary
arbVProp n gDim = frequency [ (6, fmap Ref arbitrary)
                            , (4, liftM3 Chc gDim l l)
                            , (2, fmap Not l)
                            , (2, liftM2 (&&&) l l)
                            , (2, liftM2 (|||) l l)
                            , (2, liftM2 (==>) l l)
                            , (2, liftM2 (<=>) l l)
                            ]
  where l = arbVProp (n `div` 2) gDim

-- | Generate a random prop term with no sharing among dimensions
vPropNoShare :: Arbitrary a => Gen (VProp a)
vPropNoShare = sized $ flip arbVProp genDim

-- | Generate a random prop according to its arbritrary type class instance,
-- this has a strong likelihood of sharing
-- | generate with $ x <- genVProp :: (IO (VProp String))
genVProp :: Arbitrary a => IO (VProp a)
genVProp = generate arbitrary

mkLargeVProp :: Int -> Gen (VProp a) -> Gen (VProp a)
-- mkLargeVProp = resize . (+)
mkLargeVProp = resize

----------------------------- Predicates ---------------------------------------
isPlain :: (VProp a) -> Bool
isPlain (Chc _ _ _) = False
isPlain (Not x)     = isPlain x
isPlain (Opn _ ps)  = all isPlain ps
isPlain (Op2 _ l r) = isPlain l && isPlain r
isPlain _           = True

isChc :: (VProp a) -> Bool
isChc = not . isPlain

----------------------------- Choice Manipulation ------------------------------
-- | Wrapper around engine
prune :: (VProp a) -> (VProp a)
prune = pruneTagTree Map.empty

-- | Given a config and variational expression remove redundant choices
pruneTagTree :: Config -> (VProp a) -> (VProp a)
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
pruneTagTree tb (Opn a ps)  = Opn a (pruneTagTree tb <$> ps)

-- | Given a config and a Variational VProp term select the element out that the
-- config points to
selectVariant :: Config -> (VProp a) -> Maybe (VProp a)
selectVariant _ (Ref a) = Just $ Ref a
selectVariant _ (Lit a) = Just $ Lit a
selectVariant tbs x@(Chc t y n) = case Map.lookup t tbs of
                                    Nothing    -> Just x
                                    Just True  -> selectVariant tbs y
                                    Just False -> selectVariant tbs n
selectVariant tb (Not x)     = Not <$> selectVariant tb x
selectVariant tb (Opn a ps)  = liftM (Opn a) (sequence $ selectVariant tb <$> ps)
selectVariant tb (Op2 a l r) = liftM2 (Op2 a)
                               (selectVariant tb l)
                               (selectVariant tb r)

-- | Convert a dimension to a variable
dimToVar :: Show a => (Dim -> a) -> Dim -> (VProp a)
dimToVar f = Ref . f

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: Show a => (VProp a) -> (Dim -> a) -> (VProp a)
andDecomp (Chc d l r) f = (dimToVar f d &&& andDecomp l f) |||
                        (bnot (dimToVar f d) &&& andDecomp r f)
andDecomp (Not x)     f = Not (andDecomp x f)
andDecomp (Op2 c l r) f = Op2 c (andDecomp l f) (andDecomp r f)
andDecomp (Opn c ps)  f = Opn c (flip andDecomp f <$> ps)
andDecomp x           _ = x

--------------------------- Descriptors ----------------------------------------
-- | TODO fix all this redundancy by abstracting the dimensions and instancing Bifoldable
-- | Convert a prop into a list of Terms
toList :: VProp a -> [VProp a]
toList prop = go prop []
  where
    go :: VProp a -> [VProp a] -> [VProp a]
    go x@(Not a) acc     = go a $ x:acc
    go x@(Opn _ ps) acc  = foldr' go (x:acc) ps
    go x@(Chc _ l r) acc = go r . go l $ x:acc
    go x@(Op2 _ l r) acc = go r . go l $ x:acc
    go a acc = a:acc

numTerms :: (VProp a) -> Integer
numTerms = toInteger. length . toList

-- | Count the choices in a tree
numChc :: VProp a -> Integer
numChc = toInteger . length . filter isChc . toList

-- | Count the plain values in a tree
numPlain :: VProp a -> Integer
numPlain = toInteger . length . filter isPlain . toList

-- | Given a vprop how many shared dimensions were there
numSharedDims :: VProp a -> Integer
numSharedDims = toInteger . length . filter (flip (>=) 2 . length) . group . flip go []
  where
    go :: VProp a -> [Dim] -> [Dim]
    go (Not a) acc = go a acc
    go (Op2 _ l r) acc = go l (go r acc)
    go (Opn _ ps) acc = foldr go acc ps
    go (Chc d l r) acc = go l (go r $ d:acc)
    go _    acc = acc

numSharedPlain :: Eq a => VProp a -> Integer
numSharedPlain = toInteger . length . filter (flip (>=) 2 . length) . group . filter isPlain . toList

-- | Depth of the Term tree
depth :: (VProp a) -> Integer
depth prop = go prop 0
  where
    go :: (VProp a) -> Integer -> Integer
    go (Not a) acc     = go a (succ acc)
    go (Op2 _ l r) acc = max (go l (succ acc)) (go r (succ acc))
    go (Opn _ ps)  acc = maximum $ flip go acc <$> ps
    go (Chc _ l r) acc = max (go l acc) (go r acc)
    go _ acc           = acc

-- | Given a prop return the maximum number of times a given dimension was shared
maxShared :: (VProp a) -> Integer
maxShared = toInteger . safeMaximum . fmap length . group . sort . go
  where go :: (VProp a) -> [String]
        go (Chc d l r) = (dimName d) : (go l) ++ (go r)
        go (Not l)     = go l
        go (Opn _ ps)  = foldMap go ps
        go (Op2 _ l r) = go l ++ go r
        go _           = []

        safeMaximum [] = 0
        safeMaximum xs = maximum xs

--------------------------- Destructors -----------------------------------------
-- | The set of features referenced in a feature expression.
vars :: Ord a => (VProp a) -> Set a
vars (Lit _)     = Set.empty
vars (Ref f)     = Set.singleton f
vars (Not e)     = vars e
vars (Op2 _ l r) = vars l `Set.union` vars r
vars (Opn _ ps)  = Set.unions $ vars <$> ps
vars (Chc _ l r) = vars l `Set.union` vars r

-- | The set of dimensions in a propositional expression
dimensions :: (VProp a) -> Set Dim
dimensions (Lit _)       = Set.empty
dimensions (Ref _)       = Set.empty
dimensions (Not e)       = dimensions e
dimensions (Op2 _ l r)   = dimensions l `Set.union` dimensions r
dimensions (Opn _ ps)    = Set.unions $ dimensions <$> ps
dimensions (Chc d l r)   = Set.singleton d `Set.union`
                           dimensions l `Set.union` dimensions r

-- -- | The set of all choices
configs :: (VProp a) -> [[(Dim, Bool)]]
configs prop = go (Set.toList $ dimensions prop)
  where
    go []     = [[]]
    go (d:ds) = fmap ((d, True) :) cs ++ fmap ((d, False) :) cs
          where cs = go ds


-- | Given a Variational Prop term, get all possible paths in the choice tree
paths :: (VProp a) -> Set Config
paths = Set.fromList . filter (not . Map.null) . go
  where go (Chc d l r) = do someL <- go l
                            someR <- go r
                            [Map.insert d True someL, Map.insert d False someR]
        go (Not x)     = go x
        go (Op2 _ l r) = go l <> go r
        go (Opn _ ps)  = concatMap go $ ps
        go _           = [Map.empty]

------------------------------ Manipulation ------------------------------------
-- | Given a tag tree, fmap over the tree with respect to a config
replace :: Config -> a -> (VProp a) -> (VProp a)
replace _    v (Ref _) = Ref v
replace conf v (Chc d l r) =
  case Map.lookup d conf of
    Nothing    -> Chc d (replace conf v l) (replace conf v r)
    Just True  -> Chc d (replace conf v l) r
    Just False -> Chc d l (replace conf v r)
replace conf v (Not x) = Not $ replace conf v x
replace conf v (Op2 a l r) = Op2 a (replace conf v l) (replace conf v r)
replace conf v (Opn a ps)  = Opn a (replace conf v <$> ps)
replace _    _ x           = x

recompile :: (VProp a) -> [(Config, a)] -> (VProp a)
recompile = foldr (\(conf, val) acc -> replace conf val acc)

-- | Reduce the size of a feature expression by applying some basic
--   simplification rules.
shrinkPropExpr :: (Show a, Ord a) => (VProp a) -> (VProp a)
shrinkPropExpr e
    | unsatisfiable e           = Lit False
    | tautology e               = Lit True
shrinkPropExpr (Not (Not e))    = shrinkPropExpr e
shrinkPropExpr (Opn And ps)  = Opn And (filter (not . tautology) ps)
shrinkPropExpr (Opn Or ps)   = Opn Or (filter (not . unsatisfiable) ps)
shrinkPropExpr e = e

-- | remove implications from propositional terms
eliminateImpl :: (VProp a) -> (VProp a)
eliminateImpl (Op2 Impl l r)   = (Not . eliminateImpl $ l) ||| (eliminateImpl r)
eliminateImpl (Op2 BiImpl l r) = (Not l' ||| r') &&& (l' ||| (Not r'))
  where l' = eliminateImpl l
        r' = eliminateImpl r
eliminateImpl (Not ps)      = Not (eliminateImpl ps)
eliminateImpl (Opn a ps)    = Opn a (eliminateImpl <$> ps)
eliminateImpl (Chc d l r)   = Chc d (eliminateImpl l) (eliminateImpl r)
eliminateImpl e             = e

-- | Move nots inward as much as possible
moveNotIn :: (VProp a) -> (VProp a)
moveNotIn (Not p) = case p of
  Opn And ps -> Opn Or (moveNotIn . Not <$> ps)
  Opn Or  ps -> Opn And (moveNotIn . Not <$> ps)
  Not prop   -> prop
  Chc d l r  -> Chc d r l
  prop       -> Not prop
moveNotIn (Op2 a l r) = Op2 a (moveNotIn l) (moveNotIn r)
moveNotIn (Opn a ps)  = Opn a (moveNotIn <$> ps)
moveNotIn (Chc d l r) = Chc d (moveNotIn l) (moveNotIn r)
moveNotIn x = x

-- | distribute ands over ors
distributeAndOr :: (VProp a) -> (VProp a)
distributeAndOr (Opn Or ps) = foldr1 distribute $ distributeAndOr <$> ps
  where distribute (Opn And vs) p = Opn And $ distributeAndOr <$> [ v ||| p | v <- vs]
        distribute p (Opn And vs) = Opn And $ distributeAndOr <$> [ v ||| p | v <- vs]
        distribute (Opn Or vs) p  = Opn Or $ vs ++ pure p
        distribute p (Opn Or vs)  = Opn Or $ vs ++ pure p
        distribute p q            = p ||| q
distributeAndOr (Opn a ps)  = Opn a $ distributeAndOr <$> ps
distributeAndOr (Op2 a l r) = Op2 a (distributeAndOr l) (distributeAndOr r)
distributeAndOr (Chc d l r) = Chc d (distributeAndOr l) (distributeAndOr r)
distributeAndOr (Not p)     = Not . distributeAndOr $ p
distributeAndOr p           = p

flatten :: (VProp a) -> (VProp a)
flatten (Opn And ps) = Opn And $ foldr' helper [] $ flatten <$> ps
  where helper (Opn And vs) xs = vs <> xs
        helper e          xs   = e : xs
flatten (Opn Or ps) = Opn Or $ foldr' helper [] $ flatten <$> ps
  where helper (Opn Or vs) xs = vs <> xs
        helper e          xs  = e : xs
flatten (Op2 a l r) = Op2 a (flatten l) (flatten r)
flatten (Chc d l r) = Chc d (flatten l) (flatten r)
flatten (Not p)     = Not (flatten p)
flatten e           = e

toCNF :: (VProp a) -> (VProp a)
toCNF (Chc d l r) = true &&& Chc d (toCNF l) (toCNF r)
toCNF x           = _toCNF x

_toCNF :: (VProp a) -> (VProp a)
_toCNF p
  | isCNF p = p
  | otherwise = toCNF $ fs p
  where fs = flatten . distributeAndOr . moveNotIn . eliminateImpl

isCNF :: (VProp a) -> Bool
isCNF (Opn And ps) = True && all isCNF' ps
isCNF (Chc _ l r)  = True && isCNF l && isCNF r
isCNF (Opn Or ps)  = True && all isCNF ps
isCNF (Lit _)      = True
isCNF (Ref _)      = True
isCNF (Not (Lit _)) = True
isCNF (Not (Ref _)) = True
isCNF _            = False

isCNF' :: (VProp a) -> Bool
isCNF' (Opn And _) = False
isCNF' _            = True

------------------------------ Evaluation --------------------------------------
-- TODO fix this repetition
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: (Boolean b, Mergeable b) =>
  DimBool -> VConfig a b -> (VProp a) -> b
evalPropExpr _ _  (Lit b)   = if b then true else false
evalPropExpr _ c  (Ref f)   = c f
evalPropExpr d c  (Not e)   = bnot (evalPropExpr d c  e)
evalPropExpr d c  (Opn And ps)
  = foldr1 (&&&) $ evalPropExpr d c  <$> ps
evalPropExpr d c  (Opn Or ps)
  = foldr1 (|||) $ evalPropExpr d c <$> ps
evalPropExpr d c  (Op2 Impl l r)
  = evalPropExpr d c  l ==> evalPropExpr d c r
evalPropExpr d c  (Op2 BiImpl l r)
  = evalPropExpr d c  l <=> evalPropExpr d c r
evalPropExpr d c  (Chc dim l r)
  = ite (d dim) (evalPropExpr d c l) (evalPropExpr d c r)

-- | Pretty print a feature expression.
prettyPropExpr :: (Show a) => (VProp a) -> String
prettyPropExpr = top
  where
    top :: Show a => (VProp a) -> String
    top (Opn Or ps)     = intercalate " ∨ " $ sub <$> ps
    top (Opn And ps)    = intercalate " ∧ " $ sub <$> ps
    top (Op2 Impl l r)   = sub l ++ " → " ++ sub r
    top (Op2 BiImpl l r) = sub l ++ " ↔ " ++ sub r
    top (Chc d ls rs) = show (dimName d) ++ "<" ++ top ls ++ ", " ++ top rs++ ">"
    top e           = sub e

    sub :: (Show a) => (VProp a) -> String
    sub (Lit b) = if b then "#T" else "#F"
    sub (Ref f) = show f
    sub (Not e) = "¬" ++ sub e
    sub e       = "(" ++ top e ++ ")"

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: (Show a, Ord a) => (VProp a) -> Predicate
symbolicPropExpr e = do
    let vs = Set.toList (vars e)
        ds = Set.toList (dimensions e)
    syms <- fmap (Map.fromList . zip vs) (sBools (show <$> vs))
    dims <- fmap (Map.fromList . zip ds) (sBools (map dimName ds))
    let look f = fromMaybe err (Map.lookup f syms)
        lookd d = fromMaybe errd (Map.lookup d dims)
    return (evalPropExpr lookd look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."

instance Boolean (VProp a) where
  true  = Lit True
  false = Lit False
  bnot  = Not
  l &&& r = Opn And [l, r]
  l ||| r = Opn Or [l, r]
  (==>) = Op2 Impl
  (<=>) = Op2 BiImpl

instance (Show a, Ord a) => SAT (VProp a) where
  toPredicate = symbolicPropExpr

instance Show a => Show (VProp a) where
  show = prettyPropExpr

-- | make prop mergeable so choices can use symbolic conditionals
instance Mergeable (VProp a) where
  symbolicMerge _ b thn els
    | Just result <- unliteral b = if result then thn else els
  symbolicMerge _ _ _ _ = undefined -- quite -WALL

-- | arbritrary instance for the generator monad
instance Arbitrary a => Arbitrary (VProp a) where
  arbitrary = sized $ flip arbVProp genSharedDim

-- | Deep Seq instances for Criterion Benchmarking
instance NFData a => NFData (VProp a)
instance NFData Op2
instance NFData Opn

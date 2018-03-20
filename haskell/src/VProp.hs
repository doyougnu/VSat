module VProp where

import           Control.Monad       (liftM, liftM2, liftM3)
import           Data.Data           (Data, Typeable)
import           Data.Foldable       (foldr', foldr1)
import           Data.List           (intercalate)
import           Data.Monoid         ((<>))
import           Data.String         (IsString)

import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Data.SBV
import           GHC.Generics
import           SAT

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
   | Opn Opn [VProp]
   | Op2 Op2 VProp VProp
  deriving (Data,Eq,Generic,Typeable, Show)

-- | data constructor for binary operations
data Op2 = Impl | BiImpl deriving (Eq,Generic,Data,Typeable, Show)
data Opn = And | Or deriving (Eq,Generic,Data,Typeable, Show)

-- | Generate only alphabetical characters
genAlphaNum :: Gen Char
genAlphaNum = elements ['a'..'z']

-- | generate a list of only alphabetical characters and convert to Dim
genAlphaNumStr :: Gen String
genAlphaNumStr = flip suchThat (not . null) $ listOf genAlphaNum

genDim :: Gen Dim
genDim = Dim <$> (fmap . fmap) toUpper genAlphaNumStr

genVar :: Gen Var
genVar = Var <$> genAlphaNumStr

-- | Generate an Arbitrary VProp, these frequencies can change for different
-- depths
arbVProp :: Int -> Gen VProp
arbVProp 0 = fmap Ref genVar
arbVProp n = frequency [ (1, fmap Ref genVar)
                       , (4, liftM3 Chc genDim l l)
                       , (3, fmap Not l)
                       , (3, liftM2 (&&&) l l)
                       , (3, liftM2 (|||) l l)
                       , (3, liftM2 (==>) l l)
                       , (3, liftM2 (<=>) l l)
                       ]
  where l = arbVProp (n `div` 2)

-- | Generate a random prop term according to arbVProp
genVProp :: IO VProp
genVProp = generate arbitrary

----------------------------- Predicates ---------------------------------------
isPlain :: VProp -> Bool
isPlain (Chc _ _ _) = False
isPlain (Not x)     = isPlain x
isPlain (Opn _ ps)  = all isPlain ps
isPlain (Op2 _ l r) = isPlain l && isPlain r
isPlain _           = True

isChc :: VProp -> Bool
isChc = not . isPlain

----------------------------- Choice Manipulation ------------------------------
-- | Wrapper around engine
prune :: VProp -> VProp
prune = pruneTagTree Map.empty

-- | Given a config and variational expression remove redundant choices
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
pruneTagTree tb (Opn a ps)  = Opn a (pruneTagTree tb <$> ps)

-- | Given a config and a Variational VProp term select the element out that the
-- config points to
selectVariant :: Config -> VProp -> Maybe VProp
selectVariant _ (Ref a) = Just $ Ref a
selectVariant _ (Lit a) = Just $ Lit a
selectVariant tbs (Chc t y n) = case Map.lookup t tbs of
                           Nothing    -> Nothing
                           Just True  -> selectVariant tbs y
                           Just False -> selectVariant tbs n
selectVariant tb (Not x)     = Not <$> selectVariant tb x
selectVariant tb (Opn a ps)  = liftM (Opn a) (sequence $ selectVariant tb <$> ps)
selectVariant tb (Op2 a l r) = liftM2 (Op2 a)
                               (selectVariant tb l)
                               (selectVariant tb r)

-- | Convert a dimension to a variable
dimToVar :: Dim -> VProp
dimToVar = Ref . Var . dimName

-- | Perform andDecomposition, removing all choices from a proposition
andDecomp :: VProp -> VProp
andDecomp (Chc d l r) = (dimToVar d &&& andDecomp l) |||
                        (bnot (dimToVar d) &&& andDecomp r)
andDecomp (Not x)       = Not (andDecomp x)
andDecomp (Op2 c l r) = Op2 c (andDecomp l) (andDecomp r)
andDecomp (Opn c ps)  = Opn c (andDecomp <$> ps)
andDecomp x             = x

--------------------------- Descriptors ----------------------------------------
-- | Count the terms in the expression
numTerms :: VProp -> Integer
numTerms prop = go prop 0
  where
    go :: VProp -> Integer -> Integer
    go (Not a) acc     = go a acc
    go (Op2 _ l r) acc = go r (go l acc)
    go (Opn _ ps) acc  = foldr' go acc ps
    go _       acc     = succ acc

-- | Count the choices in a tree
numChc :: VProp -> Integer
numChc prop = go prop 0
  where
    go :: VProp -> Integer -> Integer
    go (Chc _ l r) cnt = go l (succ cnt) + go r (succ cnt)
    go (Op2 _ l r) cnt = go l (go r cnt)
    go (Opn _ ps)  cnt = foldr' go cnt ps
    go (Not a)     cnt = go a cnt
    go _           cnt = cnt


-- | Depth of the Term tree
depth :: VProp -> Integer
depth prop = go prop 0
  where
    go :: VProp -> Integer -> Integer
    go (Not a) acc     = go a (succ acc)
    go (Op2 _ l r) acc = max (go l (succ acc)) (go r (succ acc))
    go (Opn _ ps)  acc = maximum $ flip go acc <$> ps
    go (Chc _ l r) acc = max (go l acc) (go r acc)
    go _ acc           = acc

--------------------------- Destructors -----------------------------------------
-- | The set of features referenced in a feature expression.
vars :: VProp -> Set Var
vars (Lit _)     = Set.empty
vars (Ref f)     = Set.singleton f
vars (Not e)     = vars e
vars (Op2 _ l r) = vars l `Set.union` vars r
vars (Opn _ ps)  = Set.unions $ vars <$> ps
vars (Chc _ l r) = vars l `Set.union` vars r

-- | The set of dimensions in a propositional expression
dimensions :: VProp -> Set Dim
dimensions (Lit _)       = Set.empty
dimensions (Ref _)       = Set.empty
dimensions (Not e)       = dimensions e
dimensions (Op2 _ l r)   = dimensions l `Set.union` dimensions r
dimensions (Opn _ ps)    = Set.unions $ dimensions <$> ps
dimensions (Chc d l r)   = Set.singleton d `Set.union`
                           dimensions l `Set.union` dimensions r

-- | The set of all choices
choices :: VProp -> Set [(Dim, Bool)]
choices prop = Set.fromList $ take n [ [(x, a), (y, b)] |
                                       x <- ds
                                       , y <- ds
                                       , a <- bs
                                       , b <- bs
                                       , x /= y
                                     ]

  where ds = Set.toList $ dimensions prop
        n  = length ds * 2
        bs = [True, False]

-- | Given a Variational Prop term, get all possible paths in the choice tree
paths :: VProp -> Set Config
paths = Set.fromList . filter (not . Map.null) . go
  where go (Chc d l r) = do someL <- go l
                            someR <- go r
                            [Map.insert d True someL, Map.insert d False someR]
        go (Not x) = go x
        go (Op2 _ l r) = go l <> go r
        go (Opn _ ps)  = concatMap go $ ps
        go _ = [Map.empty]

------------------------------ Manipulation ------------------------------------
-- | Given a tag tree, fmap over the tree with respect to a config
replace :: Config -> String -> VProp -> VProp
replace _    v (Ref _) = Ref . Var $ v
replace conf v (Chc d l r) =
  case Map.lookup d conf of
    Nothing    -> Chc d (replace conf v l) (replace conf v r)
    Just True  -> Chc d (replace conf v l) r
    Just False -> Chc d l (replace conf v r)
replace conf v (Not x) = Not $ replace conf v x
replace conf v (Op2 a l r) = Op2 a (replace conf v l) (replace conf v r)
replace conf v (Opn a ps)  = Opn a (replace conf v <$> ps)
replace _    _ x           = x

refToLit :: Var -> (Var -> Bool) -> VProp -> VProp
refToLit v f a@(Ref x) | v == x = Lit $ f x
                         | otherwise = a
refToLit v f (Not x)     = Not $ refToLit v f x
refToLit v f (Chc d l r) = Chc d (refToLit v f l) (refToLit v f r)
refToLit v f (Opn a ps)  = Opn a $ refToLit v f <$> ps
refToLit v f (Op2 a l r) = Op2 a (refToLit v f l) (refToLit v f r)
refToLit _ _ x           = x

-- | helper function used to create seed value for fold just once
-- _recompile :: Config -> String -> VProp
-- _recompile conf = go (Map.toList conf)
--   where
--     go :: [(Dim, Bool)] -> String -> VProp
--     go [] val' = Ref . Var $ val'
--     go ((d, b):cs) val'
--           | b = Chc d (go cs val') (Ref (Var "__"))
--           | otherwise = Chc d (Ref (Var "__")) (go cs val')

-- | Given a list of configs with associated values, remake the tag tree by
-- folding over the config list
-- recompile :: [(Config, String)] -> Maybe VProp
-- recompile [] = Nothing
-- recompile xs = Just $ go (tail xs') (_recompile conf (show val))
--   where
--     xs' = reverse $ sortOn (Map.size . fst) xs
--     (conf, val) = head xs'
--     go :: [(Config, String)] -> VProp -> VProp
--     go []          acc = acc
--     go ((c, v):cs) acc = go cs $ replace c (show v) acc

recompile :: VProp -> [(Config, String)] -> VProp
recompile = foldr (\(conf, val) acc -> replace conf val acc)

-- | Reduce the size of a feature expression by applying some basic
--   simplification rules.
shrinkPropExpr :: VProp -> VProp
shrinkPropExpr e
    | unsatisfiable e           = Lit False
    | tautology e               = Lit True
shrinkPropExpr (Not (Not e))    = shrinkPropExpr e
shrinkPropExpr (Opn And ps)  = Opn And (filter (not . tautology) ps)
shrinkPropExpr (Opn Or ps)   = Opn Or (filter (not . unsatisfiable) ps)
shrinkPropExpr e = e

-- | remove implications from propositional terms
eliminateImpl :: VProp -> VProp
eliminateImpl (Op2 Impl l r)   = (Not . eliminateImpl $ l) ||| (eliminateImpl r)
eliminateImpl (Op2 BiImpl l r) = (Not l' ||| r') &&& (l' ||| (Not r'))
  where l' = eliminateImpl l
        r' = eliminateImpl r
eliminateImpl (Not ps)      = Not (eliminateImpl ps)
eliminateImpl (Opn a ps)    = Opn a (eliminateImpl <$> ps)
eliminateImpl (Chc d l r)   = Chc d (eliminateImpl l) (eliminateImpl r)
eliminateImpl e             = e

-- | Move nots inward as much as possible
moveNotIn :: VProp -> VProp
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
distributeAndOr :: VProp -> VProp
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

flatten :: VProp -> VProp
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

toCNF :: VProp -> VProp
toCNF (Chc d l r) = true &&& Chc d (toCNF l) (toCNF r)
toCNF x           = _toCNF x

_toCNF :: VProp -> VProp
_toCNF p
  | isCNF p = p
  | otherwise = toCNF $ fs p
  where fs = flatten . distributeAndOr . moveNotIn . eliminateImpl

isCNF :: VProp -> Bool
isCNF (Opn And ps) = True && all isCNF' ps
isCNF (Chc _ l r)  = True && isCNF l && isCNF r
isCNF (Opn Or ps)  = True && all isCNF ps
isCNF (Lit _)      = True
isCNF (Ref _)      = True
isCNF (Not (Lit _)) = True
isCNF (Not (Ref _)) = True
isCNF _            = False

isCNF' :: VProp -> Bool
isCNF' (Opn And _) = False
isCNF' _            = True

------------------------------ Evaluation --------------------------------------
-- TODO fix this repetition
-- | Evaluate a feature expression against a configuration.
evalPropExpr :: (Boolean b, Mergeable b) => DimBool -> VConfig b -> VProp -> b
evalPropExpr _ _ (Lit b)   = if b then true else false
evalPropExpr _ c (Ref f)   = c f
evalPropExpr d c (Not e)   = bnot (evalPropExpr d c e)
evalPropExpr d c (Opn And ps)     = foldr1 (&&&) $ evalPropExpr d c <$> ps
evalPropExpr d c (Opn Or ps)      = foldr1 (|||) $ evalPropExpr d c <$> ps
evalPropExpr d c (Op2 Impl l r)   = evalPropExpr d c l ==> evalPropExpr d c r
evalPropExpr d c (Op2 BiImpl l r) = evalPropExpr d c l <=> evalPropExpr d c r
evalPropExpr d c (Chc dim l r)    = ite (d dim)
                                    (evalPropExpr d c l)
                                    (evalPropExpr d c r)

-- | Pretty print a feature expression.
prettyPropExpr :: VProp -> String
prettyPropExpr = top
  where
    top :: VProp -> String
    top (Opn Or ps)     = intercalate " ∨ " $ sub <$> ps
    top (Opn And ps)    = intercalate " ∧ " $ sub <$> ps
    top (Op2 Impl l r)   = sub l ++ " → " ++ sub r
    top (Op2 BiImpl l r) = sub l ++ " ↔ " ++ sub r
    top (Chc d ls rs) = show (dimName d) ++ "<" ++ top ls ++ ", " ++ top rs++ ">"
    top e           = sub e

    sub :: VProp -> String
    sub (Lit b) = if b then "#T" else "#F"
    sub (Ref f) = varName f
    sub (Not e) = "¬" ++ sub e
    sub e       = "(" ++ top e ++ ")"

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr :: VProp -> Predicate
symbolicPropExpr e = do
    let vs = Set.toList (vars e)
        ds = Set.toList (dimensions e)
    syms <- fmap (Map.fromList . zip vs) (sBools (map varName vs))
    dims <- fmap (Map.fromList . zip ds) (sBools (map dimName ds))
    let look f = fromMaybe err (Map.lookup f syms)
        lookd d = fromMaybe errd (Map.lookup d dims)
    return (evalPropExpr lookd look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."

instance Boolean VProp where
  true  = Lit True
  false = Lit False
  bnot  = Not
  l &&& r = Opn And [l, r]
  l ||| r = Opn Or [l, r]
  (==>) = Op2 Impl
  (<=>) = Op2 BiImpl

instance SAT VProp where
  toPredicate = symbolicPropExpr

-- instance Show VProp where
--   show = prettyPropExpr

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
instance NFData Opn

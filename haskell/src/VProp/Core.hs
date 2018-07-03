module VProp.Core where

import           Control.Monad       (liftM, liftM2)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.List           (intercalate,group,sort)
import           Data.Foldable       (foldr')
import           Data.Monoid         ((<>))
import           Prelude hiding      (LT, GT, EQ)


import VProp.Types

instance Show Var where show = varName
instance (Show a, Show b) => Show (VProp a b) where show = prettyPropExpr

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

instance Show a => Show (VIExpr a) where
  show (LitI a) = show a
  show (RefI a) = show a
  show (OpI Neg a) = "¬" <> show a
  show (OpI Abs a) = "|" <> show a <> "|"
  show (OpI Sign a) = "signum " <> show a
  show (OpII f l r) = mconcat [show l, " ", show f, " ", show r]
  show (ChcI d l r) = mconcat [show $ dimName d, "<", show l, ", ", show r, ">"]

instance Show B_B where show Not = "¬"

prettyPropExpr :: (Show a, Show b) => VProp a b -> String
prettyPropExpr = top
  where
    top :: (Show a, Show b) => VProp a b -> String
    top (Opn Or ps)     = intercalate " ∨ " $ sub <$> ps
    top (Opn And ps)    = intercalate " ∧ " $ sub <$> ps
    top (OpBB b l r)    = mconcat [sub l, " ", show b, " ", sub r]
    top (OpIB nb l r)    = mconcat [show l, " ", show nb, " ", show r]
    top (ChcB d ls rs) = show (dimName d) ++ "<" ++ top ls ++ ", " ++ top rs++ ">"
    top e           = sub e

    sub :: (Show a, Show b) => VProp a b -> String
    sub (LitB b) = if b then "#T" else "#F"
    sub (RefB f) = show f
    sub (OpB b e) = show b <> sub e
    sub e       = "(" ++ top e ++ ")"

xxx :: VProp String String
xxx =  ref "A" &&& ((iRef "b") .< (5 + (iRef "c")))

----------------------------- Predicates ---------------------------------------
-- | true if a propositions has no chcs whatsoever
-- isPlain :: VProp a -> Bool
-- isPlain (Chc _ _ _) = False
-- isPlain (OpB x)     = isPlain x
-- isPlain (Opn _ ps)  = all isPlain ps
-- isPlain (Op2 _ l r) = isPlain l && isPlain r
-- isPlain _           = True

-- isChc :: (VProp a) -> Bool
-- isChc = not . isPlain

-- ----------------------------- Choice Manipulation ------------------------------
-- -- | Wrapper around engine
-- prune :: (VProp a) -> (VProp a)
-- prune = pruneTagTree Map.empty

-- -- | Given a config and variational expression remove redundant choices
-- pruneTagTree :: Config -> (VProp a) -> (VProp a)
-- pruneTagTree _ (Ref d) = Ref d
-- pruneTagTree _ (Lit b) = Lit b
-- pruneTagTree tb (Chc t y n) = case Map.lookup t tb of
--                              Nothing -> Chc t
--                                         (pruneTagTree (Map.insert t True tb) y)
--                                         (pruneTagTree (Map.insert t False tb) n)
--                              Just True -> pruneTagTree tb y
--                              Just False -> pruneTagTree tb n
-- pruneTagTree tb (Not x)      = Not $ pruneTagTree tb x
-- pruneTagTree tb (Op2 a l r)  = Op2 a (pruneTagTree tb l) (pruneTagTree tb r)
-- pruneTagTree tb (Opn a ps)  = Opn a (pruneTagTree tb <$> ps)

-- -- | Given a config and a Variational VProp term select the element out that the
-- -- config points to
-- selectVariant :: Config -> (VProp a) -> Maybe (VProp a)
-- selectVariant _ (Ref a) = Just $ Ref a
-- selectVariant _ (Lit a) = Just $ Lit a
-- selectVariant tbs x@(Chc t y n) = case Map.lookup t tbs of
--                                     Nothing    -> Just x
--                                     Just True  -> selectVariant tbs y
--                                     Just False -> selectVariant tbs n
-- selectVariant tb (Not x)     = Not <$> selectVariant tb x
-- selectVariant tb (Opn a ps)  = liftM (Opn a) (sequence $ selectVariant tb <$> ps)
-- selectVariant tb (Op2 a l r) = liftM2 (Op2 a)
--                                (selectVariant tb l)
--                                (selectVariant tb r)

-- | Convert a dimension to a variable
dimToVar :: Show a => (Dim -> a) -> Dim -> (VProp a b)
dimToVar f = RefB . f

-- --------------------------- Descriptors ----------------------------------------
-- -- | TODO fix all this redundancy by abstracting the dimensions and instancing Bifoldable
-- -- | Convert a prop into a list of Terms
-- toList :: VProp a -> [VProp a]
-- toList prop = go prop []
--   where
--     go :: VProp a -> [VProp a] -> [VProp a]
--     go x@(Not a) acc     = go a $ x:acc
--     go x@(Opn _ ps) acc  = foldr' go (x:acc) ps
--     go x@(Chc _ l r) acc = go r . go l $ x:acc
--     go x@(Op2 _ l r) acc = go r . go l $ x:acc
--     go a acc = a:acc

-- numTerms :: (VProp a) -> Integer
-- numTerms = toInteger. length . toList

-- -- | Count the choices in a tree
-- numChc :: VProp a -> Integer
-- numChc = toInteger . length . filter isChc . toList

-- -- | Count the plain values in a tree
-- numPlain :: VProp a -> Integer
-- numPlain = toInteger . length . filter isPlain . toList

-- -- | Given a vprop how many shared dimensions were there
-- numSharedDims :: VProp a -> Integer
-- numSharedDims = toInteger . length . filter (flip (>=) 2 . length) . group . flip go []
--   where
--     go :: VProp a -> [Dim] -> [Dim]
--     go (Not a) acc = go a acc
--     go (Op2 _ l r) acc = go l (go r acc)
--     go (Opn _ ps) acc = foldr go acc ps
--     go (Chc d l r) acc = go l (go r $ d:acc)
--     go _    acc = acc

-- numSharedPlain :: Eq a => VProp a -> Integer
-- numSharedPlain = toInteger . length . filter (flip (>=) 2 . length) . group . filter isPlain . toList

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
maxShared :: VProp a b -> Int
maxShared = safeMaximum . fmap length . group . sort . go
  where go :: VProp a b -> [String]
        go (ChcB d l r) = (dimName d) : go l ++ go r
        go (OpB _ l)     = go l
        go (Opn _ ps)  = foldMap go ps
        go (OpBB _ l r) = go l ++ go r
        go (OpIB _ l r) = go' l ++ go' r
        go _           = []

        go' :: VIExpr a -> [String]
        go' (ChcI d l r) = (dimName d) : go' l ++ go' r
        go' (OpI _ l)    = go' l
        go' (OpII _ l r) = go' l ++ go' r
        go' _            = []

        safeMaximum [] = 0
        safeMaximum xs = maximum xs

-- --------------------------- Destructors -----------------------------------------
-- | The set of features referenced in a feature expression.
vars :: Ord a => (VProp a a) -> Set.Set a
vars (LitB _)     = Set.empty
vars (RefB f)     = Set.singleton f
vars (OpB _ e)    = vars e
vars (OpBB _ l r) = vars l `Set.union` vars r
vars (OpIB _ l r) = vars' l `Set.union` vars' r
vars (Opn _ ps)   = Set.unions $ vars <$> ps
vars (ChcB _ l r) = vars l `Set.union` vars r

vars' :: Ord a => (VIExpr a) -> Set.Set a
vars' (LitI _) = Set.empty
vars' (RefI f) = Set.singleton f
vars' (OpI _ e) = vars' e
vars' (OpII _ l r) = vars' l `Set.union` vars' r
vars' (ChcI _ l r) = vars' l `Set.union` vars' r

-- | The set of dimensions in a propositional expression
dimensions :: (VProp a b) -> Set.Set Dim
dimensions (LitB _)     = Set.empty
dimensions (RefB _)     = Set.empty
dimensions (OpB _ e)    = dimensions e
dimensions (OpBB _ l r) = dimensions l `Set.union` dimensions r
dimensions (OpIB _ l r) = dimensions' l `Set.union` dimensions' r
dimensions (Opn _ ps)   = Set.unions $ dimensions <$> ps
dimensions (ChcB d l r) = Set.singleton d `Set.union`
                            dimensions l `Set.union` dimensions r

dimensions' :: (VIExpr a) -> Set.Set Dim
dimensions' (LitI _)     = Set.empty
dimensions' (RefI _)     = Set.empty
dimensions' (OpI _ e)    = dimensions' e
dimensions' (OpII _ l r) = dimensions' l `Set.union` dimensions' r
dimensions' (ChcI d l r) = Set.singleton d `Set.union`
                             dimensions' l `Set.union` dimensions' r

-- | The set of integar variable references for an expression
ivars :: (Ord a, Ord b) => VProp a b -> Set.Set b
ivars (LitB _)     = Set.empty
ivars (RefB _)     = Set.empty
ivars (OpB _ e)    = ivars e
ivars (OpBB _ l r) = ivars l `Set.union` ivars r
ivars (OpIB _ l r) = ivars' l `Set.union` ivars' r
ivars (Opn _ ps)   = Set.unions $ ivars <$> ps
ivars (ChcB _ l r) = ivars l `Set.union` ivars r

ivars' :: Ord a => VIExpr a -> Set.Set a
ivars' (LitI _)     = Set.empty
ivars' (RefI a)     = Set.singleton a
ivars' (OpI _ e)    = ivars' e
ivars' (OpII _ l r) = ivars' l `Set.union` ivars' r
ivars' (ChcI _ l r) = ivars' l `Set.union` ivars' r


-- | The set of all choices
configs :: VProp a b -> [[(Dim, Bool)]]
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

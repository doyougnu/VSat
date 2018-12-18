module CaseStudy.Auto.Auto where

import qualified Control.Monad.State.Strict as S
import           Data.Aeson
import           Data.Bifunctor             (bimap)
import qualified Data.Map                   as M
import qualified Data.Sequence              as SE
import           Data.String                (IsString)
import           Data.Text

import           CaseStudy.Auto.Lang
import qualified VProp.Types                as V
import VProp.Core()

import Debug.Trace (trace)

-- | A context represents an evolution context which are temporal bounds
-- represented as integers
type Context = (Integer, Integer)
type EvoContext a = (RBOp, ALang a)

-- ^ mirrored maps to track if a dimension should be generated or not
type DimMap a = M.Map (EvoContext a) V.Dim

-- ^ State for tagging evolution contexts to unique dimensions
type Annot a = S.State (DimMap a, Integer)

-- | The auto type encodes the context of the automotive encoding, and the
-- constraints that range over the features in the automotive model
data Auto = Auto { contexts    :: Context
                 , constraints :: [Text]
                 }
          deriving Show

instance FromJSON Auto where
  parseJSON = withObject "someJSON" $ \o -> do
    -- get the min and max
    [m] <- o .: "contexts"
    mn <- m .: "min"
    mx <- m .: "max"

    -- constraints
    constraints <- o .: "constraints"

    return Auto{contexts=(mn,mx), constraints=constraints}

-- | run the state monad and get a vprop expression back
autoToVSat :: (IsString a, Show a, Eq a, Ord a) => AutoLang a -> V.VProp a a
autoToVSat = fst . runAutoToVSat__

runAutoToVSat__ :: (IsString a, Show a, Eq a, Ord a) =>
  AutoLang a -> (V.VProp a a, (DimMap a, Integer))
runAutoToVSat__ = flip S.runState (M.empty, 0) . autoToVSat_

-- | A hole that is used as a placeholder for reifying nested choices in
-- autoToVsat
hole :: (IsString a, Show a, Eq a, Ord a) => V.VProp a b
hole = V.RefB "_"

isHole :: (IsString a, Show a, Eq a, Eq b, Ord a) => V.VProp a b -> Bool
isHole = (==) hole

has :: (IsString a, Show a, Show b, Eq a, Eq b, Ord a) => (V.VProp a b -> Bool) -> V.VProp a b -> Bool
has p a@(V.Opn  _ es)  = p a || Prelude.foldr (\x acc -> p x || acc) False es
has p a@(V.OpB  _ e)   = p a || has p e
has p a@(V.OpBB _ l r) = p a || has p l || has p r
has p a@(V.ChcB _ l r) = p a || has p l || has p r
has p x = p x

hasHole :: (IsString a, Show a, Show b, Eq a, Eq b, Ord a) => V.VProp a b -> Bool
hasHole = has isHole

-- | convert an autolang expression to a vprop lang expression. State monad to
-- keep track of which evolution contexts have been observed and which
-- dimensions are assigned to those evo contexts
autoToVSat_ :: (IsString a, Show a, Eq a, Ord a) =>
  AutoLang a -> Annot a (V.VProp a a)
autoToVSat_ (AutoLit a) = return $ V.LitB a
  -- singleton contexts can be converted to naive encoding directly
autoToVSat_ (Ctx op aexpr boolexpr) =
  do (evos, i) <- S.get
     let newDim = V.Dim $ "D_" ++ show i
         evoRng = (op, aexpr)
     dim <- case (evoRng `M.lookup` evos) of
              -- this is a not yet observed evoRng
              Nothing -> do S.modify $ bimap (M.insert evoRng newDim) succ
                            return newDim
              -- a repeated entry so just return it
              (Just a) -> return a
     flip (V.ChcB dim) (V.LitB True) <$> (autoToVSat_ boolexpr)
  -- compound contexts need a two step process, first encode the relation into a
  -- choice with holes. Then call reify to generate the nested choices
autoToVSat_ (RBinary op (ACtx _) rhs) =
  do (evos, i) <- S.get
     let newDim = V.Dim $ "D_" ++ show i
         evoRng = (op, rhs)
     dim <- case (evoRng `M.lookup` evos) of
              -- this is a not yet observed evoRng
              Nothing -> do S.modify $ bimap (M.insert evoRng newDim) succ
                            return newDim
              -- a repeated entry so just return it
              (Just a) -> return a
     return $ V.ChcB dim hole hole
autoToVSat_ (AutoRef a) = return $ V.RefB a
autoToVSat_ (AutoNot a) = V.OpB V.Not <$> autoToVSat_ a
autoToVSat_ (BBinary And l r) = V.Opn V.And <$>
                                traverse autoToVSat_ (l SE.<| (SE.singleton r))
autoToVSat_ (BBinary Or l r) = V.Opn V.Or <$>
                               traverse autoToVSat_ (l SE.<| (SE.singleton r))
autoToVSat_ (BBinary op l r) = V.OpBB (dispatch op) <$>
                               (autoToVSat_ l) <*> (autoToVSat_ r)
autoToVSat_ (RBinary op l r) = return $ V.OpIB
                               (dispatch' op) (autoToVSat' l) (autoToVSat' r)

-- | sister function to the non-ticked version for handling the arithmetic sub
-- lang
autoToVSat' :: Show a => ALang a -> V.VIExpr a
autoToVSat' (ALit i) = V.LitI $ V.I i
autoToVSat' (AVar a) = V.Ref V.RefI a
autoToVSat' (CaseStudy.Auto.Lang.Neg a) = V.OpI V.Neg $ autoToVSat' a
autoToVSat' (ABinary op l r) = V.OpII (dispatch'' op) (autoToVSat' l) (autoToVSat' r)
autoToVSat' (ACtx _) = error "[ERR in AutoToVSat': ACtx found in pattern match but AutoToVSat should have prevented this matching. Send the missiles!]"

-- | Dispatch functions for operators in the autolang AST. we leave And and Or
-- undefined because they will never be called. This is required to convert
-- between the binary tree And/Or in AutoLang to the n-ary And/Or in VProp Lang
dispatch :: BOp -> V.BB_B
dispatch And  =  undefined
dispatch Or   =  undefined
dispatch Impl = V.Impl
dispatch Eqv  = V.BiImpl
dispatch Xor  = V.XOr

dispatch' :: RBOp -> V.NN_B
dispatch' GRT  = V.GT
dispatch' GRTE = V.GTE
dispatch' EQL  = V.EQ
dispatch' LST  = V.LT
dispatch' LSTE = V.LTE
dispatch' NEQL = V.NEQ

dispatch'' :: AOp -> V.NN_N
dispatch'' Add      = V.Add
dispatch'' Subtract = V.Sub
dispatch'' Multiply = V.Mult
dispatch'' Divide   = V.Div
dispatch'' Modulus  = V.Mod

-- | take a list of autolangs queries and conjoin them as a single query. The
-- conjunction is domain specific and appropriate in this context
conjoin :: [AutoLang a] -> AutoLang a
conjoin = Prelude.foldr1 (BBinary And)

disjoin :: [AutoLang a] -> AutoLang a
disjoin = Prelude.foldr1 (BBinary Or)

-- | Take a VProp term that has choices with holes and reify them to the simple
-- encoding
nestChoices :: (IsString a, Show a, Eq a, Ord a) => V.VProp a a -> V.VProp a a
  -- base case to prevent an Opn op empty list at end of recursion
nestChoices (V.Opn _ (a@(V.ChcB _ _ _) SE.:<| SE.Empty)) = a

  -- any choice that maintains a hole is transformed into a nested choice
nestChoices (V.Opn op (a@(V.ChcB dim l r) SE.:<| xs))
  | l == hole && r == hole = V.ChcB dim (nestChoices (V.Opn op xs)) (V.true)
  | otherwise = V.Opn V.And $ a SE.:<| (nestChoices <$> xs)

  -- recursive cases
nestChoices (V.OpB o os) = V.OpB o $ nestChoices os
nestChoices (V.OpBB o l r) = V.OpBB o (nestChoices l) (nestChoices r)
nestChoices (V.Opn o ps) = V.Opn o $ nestChoices <$> ps
nestChoices (V.ChcB d l r) = V.ChcB d (nestChoices l) (nestChoices r)
nestChoices x = x

-- | Fill holes given a predicate, an old vprop, and a replacement vprop
fillBy :: (Show a, Show b) => (V.VProp a b -> Bool) -> V.VProp a b -> V.VProp a b-> V.VProp a b
fillBy p a@(V.OpB op e) new
  | p a = new
  | otherwise = trace ("[DBG OpB]: " ++ show a) $ V.OpB  op  (fillBy p e new)
fillBy p a@(V.OpBB op l r)  new
  | p a = new
  | otherwise = trace ("[DBG OpBB]: " ++ show a) $ V.OpBB op  (fillBy p l new) (fillBy p r new)
fillBy p a@(V.Opn op ps) new
  | p a = new
  | otherwise = trace ("[DBG Opn]: " ++ show a) $ V.Opn  op  $ flip (fillBy p) new <$> ps
fillBy p a@(V.ChcB dim l r) new
  | p a = new
  | otherwise = trace ("[DBG Chc]" ++ show a) $ V.ChcB dim (fillBy p l new) (fillBy p r new)
fillBy p x new
  | p x = new
  | otherwise = x

-- | fill holes by identifying them with isHole predicate function
fill :: (IsString a, Eq a, Eq b, Ord a, Show a, Show b) =>
  V.VProp a b -> V.VProp a b -> V.VProp a b
fill = fillBy isHole

naiveEncode :: (IsString a, Show a, Eq a, Ord a) => V.VProp a a -> V.VProp a a
naiveEncode (V.OpBB op a@(V.ChcB dim l r) rest)
  | hasHole a = fill a rest
  | otherwise = V.OpBB op
                (V.ChcB dim (naiveEncode l) (naiveEncode r))
                (naiveEncode rest)
naiveEncode (V.OpBB op l r) = V.OpBB op (naiveEncode l) (naiveEncode r)
naiveEncode (V.OpB op e) = V.OpB op (naiveEncode e)
naiveEncode (V.Opn op es) = V.Opn op $ naiveEncode <$> es
naiveEncode (V.ChcB d l r) = V.ChcB d (naiveEncode l) (naiveEncode r)
naiveEncode nonrecursive = nonrecursive

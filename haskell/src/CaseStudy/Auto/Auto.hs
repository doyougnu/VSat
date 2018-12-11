module CaseStudy.Auto.Auto where

import Data.Aeson
import Data.Text
import qualified Control.Monad.State.Strict as S
import qualified Data.Sequence as SE
import qualified Data.Map as M
import Data.Bifunctor (bimap)

import qualified VProp.Types as V
import CaseStudy.Auto.Lang

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
data Auto = Auto { contexts :: Context
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
autoToVSat :: (Show a, Eq a, Ord a) => AutoLang a -> V.VProp a a
autoToVSat = fst . autoToVSat__

autoToVSat__ :: (Show a, Eq a, Ord a) =>
  AutoLang a -> (V.VProp a a, (DimMap a, Integer))
autoToVSat__ = flip S.runState (M.empty, 0) . autoToVSat_

-- | convert an autolang expression to a vprop lang expression. State monad to
-- keep track of which evolution contexts have been observed and which
-- dimensions are assigned to those evo contexts
autoToVSat_ :: (Show a, Eq a, Ord a) => AutoLang a -> Annot a (V.VProp a a)
autoToVSat_ (AutoLit a) = return $ V.LitB a
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

-- | Dispatch functions for operators in the autolang AST. we leave And and Or
-- undefined because they will never be called. This is required to convert
-- between the binary tree And/Or in AutoLang to the n-ary And/Or in VProp Lang
dispatch :: BOp -> V.BB_B
dispatch And =  undefined
dispatch Or =  undefined
dispatch Impl = V.Impl
dispatch Eqv = V.BiImpl
dispatch Xor = V.XOr

dispatch' :: RBOp -> V.NN_B
dispatch' GRT = V.GT
dispatch' GRTE = V.GTE
dispatch' EQL = V.EQ
dispatch' LST = V.LT
dispatch' LSTE = V.LTE
dispatch' NEQL = V.NEQ

dispatch'' :: AOp -> V.NN_N
dispatch'' Add = V.Add
dispatch'' Subtract = V.Sub
dispatch'' Multiply = V.Mult
dispatch'' Divide = V.Div
dispatch'' Modulus = V.Mod

-- | take a list of autolangs queries and conjoin them as a single query. The
-- conjunction is domain specific and appropriate in this context
conjoin :: [AutoLang a] -> AutoLang a
conjoin = Prelude.foldr1 (BBinary And)

disjoin :: [AutoLang a] -> AutoLang a
disjoin = Prelude.foldr1 (BBinary Or)

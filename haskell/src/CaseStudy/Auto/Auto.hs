module CaseStudy.Auto.Auto where

import qualified Control.Monad.State.Strict as S
import           Data.Aeson
import           Data.Bifunctor             (bimap)
import           Data.Bifoldable            (bifoldMap)
import           Data.Monoid                (Sum(..))
import qualified Data.Map                   as M
import           Data.String                (IsString)
import           Data.Text

import           CaseStudy.Auto.Lang
import           Utils                      (fromList, fromList')
import           VProp.Core                 ()
import qualified VProp.Types                as V

-- | A context represents an evolution context which are temporal bounds
-- represented as integers
type Context = (Integer, Integer)
type EvoContext a = (RBOp, ALang a)

-- ^ mirrored maps to track if a dimension should be generated or not
type DimMap a b = M.Map (EvoContext a) (V.Dim b)

-- ^ The state for annotating the autolang to prop. We keep track of dimensions
-- that have been seen before and a counter to generate unique dimension names
-- when we see a new dimension
type AnnotSt a = (DimMap a Text, Integer)

-- ^ State for tagging evolution contexts to unique dimensions
type Annot a = S.State (AnnotSt a)

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
autoToVSat :: (IsString a, Show a, Eq a, Ord a, Monoid a) =>
  AutoLang a a -> V.VProp Text a a
autoToVSat = fst . runAutoToVSat'

getDimMap :: (IsString a, Show a, Eq a, Ord a, Monoid a) =>
  AutoLang a a -> DimMap a Text
getDimMap = fst . snd. runAutoToVSat'

runAutoToVSat' :: (IsString a, Show a, Eq a, Ord a, Monoid a) =>
  AutoLang a a -> (V.VProp Text a a, (DimMap a Text, Integer))
runAutoToVSat' = flip S.runState (M.empty, 0) . autoToVSat_

-- | A hole that is used as a placeholder for reifying nested choices in
-- autoToVsat
hole :: (IsString a, Eq a, Ord a) => V.VProp d a b
hole = V.RefB "_"

isHole :: (IsString a, Eq a, Eq b, Eq d, Ord a) => V.VProp d a b -> Bool
isHole = (==) hole

has :: (IsString a, Eq a, Eq b, Ord a) =>
  (V.VProp d a b -> Bool) -> V.VProp d a b -> Bool
has p a@(V.OpB  _ e)   = p a || has p e
has p a@(V.OpBB _ l r) = p a || has p l || has p r
has p a@(V.ChcB _ l r) = p a || has p l || has p r
has p x                = p x

hasHole :: (IsString a, Eq a, Eq b, Ord a, Eq d) =>
  V.VProp d a b -> Bool
hasHole = has isHole

-- | convert an autolang expression to a vprop lang expression. State monad to
-- keep track of which evolution contexts have been observed and which
-- dimensions are assigned to those evo contexts
-- singleton contexts can be converted to naive encoding directly
autoToVSat_ :: (Show c, IsString c, S.MonadState (AnnotSt c) m, Ord c) =>
               AutoLang c c -> m (V.VProp Text c c)
autoToVSat_ (AutoLit a) = return $ V.LitB a
autoToVSat_ (Ctx op aexpr boolexpr) =
  do (evos, i) <- S.get
     let newDim = V.Dim $ mconcat ["D_", pack $ show i]
           -- mconcat ["D_", "(", pack $ show op, "_", pack $ show aexpr, ")", "_", pack $ show i]
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
     let newDim = V.Dim $ mconcat ["D_", pack $ show i]
           -- mconcat ["D_", "(", pack $ show op, "_", pack $ show rhs, ")", "_", pack $ show i]
         evoRng = (op, rhs)
     dim <- case (evoRng `M.lookup` evos) of
              -- this is a not yet observed evoRng
              Nothing -> do S.modify $ bimap (M.insert evoRng newDim) succ
                            return newDim
              -- a repeated entry so just return it
              (Just a) -> return a
     return $ V.ChcB dim hole V.true
autoToVSat_ (AutoRef a) = return $ V.RefB a
autoToVSat_ (AutoNot a) = V.OpB V.Not <$> autoToVSat_ a
autoToVSat_ (BBinary op l r) = V.OpBB (dispatch op) <$>
                               (autoToVSat_ l) <*> (autoToVSat_ r)
autoToVSat_ (RBinary op l r) = return $ V.OpIB
                               (dispatch' op) (autoToVSat' l) (autoToVSat' r)

-- | sister function to the non-ticked version for handling the arithmetic sub
-- lang
autoToVSat' :: Show a => ALang a -> V.VIExpr d a
autoToVSat' (ALit i) = V.LitI $ V.I $ fromInteger i
autoToVSat' (AVar a) = V.Ref V.RefI a
autoToVSat' (CaseStudy.Auto.Lang.Neg a) = V.OpI V.Neg $ autoToVSat' a
autoToVSat' (ABinary op l r) = V.OpII (dispatch'' op) (autoToVSat' l) (autoToVSat' r)
autoToVSat' (ACtx _) = error "[ERR] in AutoToVSat': ACtx found in pattern match but AutoToVSat should have prevented this matching. Send the missiles!"

-- | Dispatch functions for operators in the autolang AST. we leave And and Or
-- undefined because they will never be called. This is required to convert
-- between the binary tree And/Or in AutoLang to the n-ary And/Or in VProp Lang
dispatch :: BOp -> V.BB_B
dispatch And  = V.And
dispatch Or   = V.Or
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

-- | Take a VProp term that has choices with holes and reify them to the simple
-- encoding
nestChoices :: (IsString a, Eq a, Ord a, Eq d) =>
  V.VProp d a a -> V.VProp d a a
  -- base case to prevent an Opn op empty list at end of recursion
  -- any choice that maintains a hole is transformed into a nested choice
nestChoices (V.OpBB o (V.ChcB dim l r) r')
  | l == hole && r == V.true = V.ChcB dim (nestChoices r') (V.true)
  | otherwise = V.OpBB o (V.ChcB dim (nestChoices l) (nestChoices r))
                          (nestChoices r')

nestChoices (V.OpBB o l' (V.ChcB dim l r))
  | l == hole && r == V.true = V.ChcB dim (nestChoices l') (V.true)
  | otherwise = V.OpBB o (nestChoices l')
                (V.ChcB dim (nestChoices l) (nestChoices r))

  -- recursive cases
nestChoices (V.OpB o os) = V.OpB o $ nestChoices os
nestChoices (V.OpBB o l r) = V.OpBB o (nestChoices l) (nestChoices r)
nestChoices (V.ChcB d l r) = V.ChcB d (nestChoices l) (nestChoices r)
nestChoices x = x

-- | Fill holes given a predicate, an old vprop, and a replacement vprop
fillBy ::
  (V.VProp d a b -> Bool) ->
  V.VProp d a b           ->
  V.VProp d a b           ->
  V.VProp d a b
fillBy p a@(V.OpB op e) new
  | p a = new
  | otherwise = V.OpB  op  (fillBy p e new)
fillBy p a@(V.OpBB op l r)  new
  | p a = new
  | otherwise = V.OpBB op  (fillBy p l new) (fillBy p r new)
fillBy p a@(V.ChcB dim l r) new
  | p a = new
  | otherwise = V.ChcB dim (fillBy p l new) (fillBy p r new)
fillBy p x new
  | p x = new
  | otherwise = x

-- | fill holes by identifying them with isHole predicate function
fill :: (IsString a, Eq a, Eq b, Eq d, Ord a) =>
  V.VProp d a b -> V.VProp d a b -> V.VProp d a b
fill = fillBy isHole

naiveEncode :: (IsString a, Eq a, Ord a, Eq d) => V.VProp d a a -> V.VProp d a a
naiveEncode (V.OpBB V.Impl a@(V.ChcB dim l r) r')
  | hasHole a = fill a r'
  | otherwise = V.OpBB V.Impl
                (V.ChcB dim (naiveEncode l) (naiveEncode r))
                (naiveEncode r')

naiveEncode (V.OpBB V.Impl l r')
  | hasHole l = fill l r'
  | otherwise = V.OpBB V.Impl
                (naiveEncode l)
                (naiveEncode r')

naiveEncode (V.OpBB op l r) = V.OpBB op (naiveEncode l) (naiveEncode r)
naiveEncode (V.OpB op e) = V.OpB op (naiveEncode e)
naiveEncode (V.ChcB d l r) = V.ChcB d (naiveEncode l) (naiveEncode r)
naiveEncode nonrecursive = nonrecursive

-- | Compressed encoding, reasons
-- about the operators and uses the
-- false leaves of the choices
compEncode :: (IsString a, Eq a, Ord a, Eq d) => DimMap a Text -> V.VProp d a a -> V.VProp d a a
compEncode dimMap (V.OpBB V.Impl a@(V.ChcB dim l r) r')
  | hasHole a = fill a r'
  | otherwise = V.OpBB V.Impl
                (V.ChcB dim (compEncode dimMap l) (compEncode dimMap r))
                (compEncode dimMap r')

compEncode dimMap (V.OpBB V.Impl l' a@(V.ChcB dim l r))
  | hasHole a = fill a l'
  | otherwise = V.OpBB V.Impl
                (compEncode dimMap l')
                (V.ChcB dim
                 (compEncode dimMap l) (compEncode dimMap r))

compEncode dimMap (V.OpBB op l r) = V.OpBB op (compEncode dimMap l) (compEncode dimMap r)
compEncode dimMap (V.OpB op e) = V.OpB op (compEncode dimMap e)
compEncode dimMap (V.ChcB d l r) = V.ChcB d (compEncode dimMap l) (compEncode dimMap r)
compEncode _ nonrecursive = nonrecursive

-- | An identical encoding just removes contexts and treats the language as
-- plain
idEncode :: (IsString a, IsString b) => AutoLang a b -> AutoLang a b
idEncode (Ctx op expr rest) = (BBinary And
                               (RBinary op (AVar "evo_ctx") expr)
                                (idEncode rest))
idEncode (RBinary op l r)   = RBinary op (idEncode' l) (idEncode' r)
idEncode (BBinary op l r)   = BBinary op (idEncode l) (idEncode r)
idEncode (AutoNot e)        = AutoNot $ idEncode e
idEncode x                  = x

idEncode' :: ALang a -> ALang a
idEncode' (ACtx expr) = expr
idEncode' x           = x

autoAndJoin :: [AutoLang a b] -> AutoLang a b
autoAndJoin = fromList $ BBinary And

autoAndJoin' :: [AutoLang a b] -> AutoLang a b
autoAndJoin' = fromList' $ BBinary And

autoLength :: AutoLang a b -> Int
autoLength = getSum . bifoldMap (Sum . const 1) (Sum . const 1)

vPropToAuto :: V.VProp a a a -> AutoLang a a
vPropToAuto (V.LitB b) = AutoLit b
vPropToAuto (V.RefB r) = AutoRef r
vPropToAuto (V.OpB V.Not e) = AutoNot $ vPropToAuto e
vPropToAuto (V.OpBB op l r) =
  BBinary (rdispatch op) (vPropToAuto l) (vPropToAuto r)
vPropToAuto (V.ChcB d l r) = (BBinary And
                              (BBinary Impl
                              (RBinary LSTE
                                (ACtx (AVar (V.dimName d)))
                                (ALit 0)) (vPropToAuto l))
                              (BBinary Impl
                              (RBinary LSTE
                                (ACtx (AVar (V.dimName d)))
                                (ALit 0)) (vPropToAuto r)))

rdispatch :: V.BB_B -> BOp
rdispatch V.And    = And
rdispatch V.Or     = Or
rdispatch V.Impl   = Impl
rdispatch V.BiImpl = Eqv
rdispatch V.XOr    = Xor

rdispatch' :: BOp -> V.BB_B
rdispatch' And    = V.And
rdispatch' Or     = V.Or
rdispatch' Impl   = V.Impl
rdispatch' Eqv    = V.BiImpl
rdispatch' Xor    = V.XOr

idispatch :: RBOp -> V.NN_B
idispatch LST  = V.LT
idispatch EQL  = V.EQ
idispatch GRT  = V.GT
idispatch LSTE = V.LTE
idispatch GRTE = V.GTE
idispatch NEQL = V.NEQ


breakOnAnd :: AutoLang a a -> [AutoLang a a]
breakOnAnd (BBinary And l r) = Prelude.concat [breakOnAnd l, breakOnAnd r]
breakOnAnd x = [x]

-- where hasCtxForm (RBinary _ (ACtx _) _) = True
-- (RBinary op (ACtx (AVar a)) (ALit i))

-- | This is a custom function to fix a problem in the initial encoding where we
-- are considering <=1 and <2 to different when they are in fact identical
-- similarly for <=0 and <1
renameCtxs :: (AutoLang a a -> AutoLang a a) -> AutoLang a a -> AutoLang a a
renameCtxs f (AutoNot e) = AutoNot $ renameCtxs f e
renameCtxs f (BBinary op l r) = BBinary op (renameCtxs f l) (renameCtxs f r)
renameCtxs f x@(RBinary _ (ACtx _) (ALit _)) = f x
renameCtxs _ nonrecursive    = nonrecursive

-- | Encode that <=1 is <2 and <=0 < 1
sameCtxs :: AutoLang a a -> AutoLang a a
sameCtxs (RBinary LSTE x@(ACtx _) (ALit 1)) = RBinary LST x (ALit 2)
sameCtxs (RBinary LSTE x@(ACtx _) (ALit 0)) = RBinary LST x (ALit 1)
sameCtxs x = x

-- | Given an AutoLang a a, remove conjuncted identical nodes incurred by fixing
-- the contexts
simplifyCtxs :: Eq a => AutoLang a a -> AutoLang a a
simplifyCtxs (BBinary And l r)
  | l == r = l
  | otherwise = BBinary And (simplifyCtxs l) (simplifyCtxs r)
simplifyCtxs (AutoNot e)      = AutoNot $ simplifyCtxs e
simplifyCtxs (BBinary op l r) = BBinary op (simplifyCtxs l) (simplifyCtxs r)
simplifyCtxs nonrecursive     = nonrecursive

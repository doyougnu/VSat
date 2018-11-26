module CaseStudy.Auto.Auto where

import Data.Aeson
import Data.Text

import qualified VProp.Types as V
import VProp.Core
import CaseStudy.Auto.Lang
import CaseStudy.Auto.Parser (langParser, contextRef)

import Data.ByteString.Lazy.Internal

-- import Api

-- | A context represents an evolution context which are temporal bounds
-- represented as integers
type Context = (Integer, Integer)

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

d' :: AutoLang Text
d' = Ctx LST (ALit 0) (BBinary Impl (RBinary EQL (AVar "a63857d6-2617-4a9d-911d-1f59aa423d49") (ALit 1)) (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (BBinary And (RBinary EQL (AVar "b493ac45-c0a8-4c93-a622-98a3d5a981a8") (ALit 1)) (RBinary EQL (AVar "0e8e9baa-56f5-48d3-93dd-1f4db1d546d4") (ALit 1))) (RBinary EQL (AVar "6aa42cc5-013e-4b44-b75b-09c55c90e8a7") (ALit 1))) (RBinary EQL (AVar "1f3c4f9e-fc8a-44bb-91de-33fdf84ad535") (ALit 1))) (RBinary EQL (AVar "84928c30-724e-4e73-b9fb-733518d0e3c6") (ALit 1))) (RBinary EQL (AVar "53e5b7e7-7ae7-44cd-a740-8d993d7eb86a") (ALit 1))) (RBinary EQL (AVar "e310f7d9-7ad6-4732-8fb2-ae3e24a9a7e5") (ALit 1))) (RBinary EQL (AVar "c4ca2f88-0407-40b0-910d-241327bde72c") (ALit 1))) (RBinary EQL (AVar "2ad480a4-eb1b-4846-bb97-00595e23faf3") (ALit 1))) (RBinary EQL (AVar "375ddff0-aa5a-44d8-8545-d5fb62fd53e8") (ALit 1))) (RBinary EQL (AVar "47fca63b-434a-46d7-9c3c-8b77eeb86d5b") (ALit 1))) (RBinary EQL (AVar "a10b3970-245e-4f43-869f-0b0f48f9390b") (ALit 1))) (RBinary EQL (AVar "67419fde-def3-4dc9-8124-5a7e76600e33") (ALit 1))) (RBinary EQL (AVar "c7f5b43a-d84b-486b-bafa-4eabc1020ba1") (ALit 1))) (RBinary EQL (AVar "9fe16054-55b2-4482-b6ec-d10ffc9e4e6e") (ALit 1))) (RBinary EQL (AVar "e6460757-4ba6-40e9-a348-c236e34c6a0c") (ALit 1))) (RBinary EQL (AVar "30b661fe-1318-4dab-ad0d-8fbfdee925ee") (ALit 1))) (RBinary EQL (AVar "dc6635d4-7e7c-4b67-9e4e-84a0cfc0a9ac") (ALit 1))) (RBinary EQL (AVar "934798c5-75f7-4894-8202-8b28577cca22") (ALit 1))) (RBinary EQL (AVar "4cadf655-6436-45ef-8d1a-5275b2b71fe9") (ALit 1))) (RBinary EQL (AVar "85ce67eb-cbd6-4256-b6a6-3558abf370f0") (ALit 1))) (RBinary EQL (AVar "33125359-6c3d-4953-8448-9a9b49f38fd4") (ALit 1))) (RBinary EQL (AVar "32e4bd1a-f62f-4f71-a3b6-6cb0ff8e2deb") (ALit 1))) (RBinary EQL (AVar "3ce915e1-c65e-4b1e-bd3e-57f2f517ad40") (ALit 1))) (RBinary EQL (AVar "9a75ad02-8c39-4d4c-8232-5d49f146dbd9") (ALit 1))) (RBinary EQL (AVar "52d44fbd-d580-4dcc-bed9-8fb1c960b34d") (ALit 1))) (RBinary EQL (AVar "53c1023e-584e-42cf-998d-5a6ff50dc237") (ALit 1))) (RBinary EQL (AVar "cf79eb43-6421-43d2-818b-4fefff12ff3d") (ALit 1))) (RBinary EQL (AVar "47a3155b-fda9-45f0-8fae-a557ead35d06") (ALit 1))) (RBinary EQL (AVar "ca068bac-3dd8-4503-9506-db7198e9ae7d") (ALit 1))) (RBinary EQL (AVar "b47c8cb0-0b60-46dd-b2c5-5853a5fc647d") (ALit 1))))

autoToVSat :: Show a => AutoLang a -> V.VProp a a
autoToVSat (AutoLit a) = V.LitB a
autoToVSat (Ctx op a b) = V.ChcB "AA" (autoToVSat b) (V.LitB True)
autoToVSat (AutoRef a) = V.RefB a
autoToVSat (AutoNot a) = V.OpB V.Not $ autoToVSat a
autoToVSat (BBinary And l r) = V.Opn V.And $ autoToVSat <$> [l,r]
autoToVSat (BBinary Or l r) = V.Opn V.Or $ autoToVSat <$> [l,r]
autoToVSat (BBinary op l r) = V.OpBB (dispatch op) (autoToVSat l) (autoToVSat r)
autoToVSat (RBinary op l r) = V.OpIB (dispatch' op) (autoToVSat' l) (autoToVSat' r)

autoToVSat' :: Show a => ALang a -> V.VIExpr a
autoToVSat' (ALit i) = V.LitI $ V.I i
autoToVSat' (AVar a) = V.Ref V.RefI a
autoToVSat' (CaseStudy.Auto.Lang.Neg a) = V.OpI V.Neg $ autoToVSat' a
autoToVSat' (ABinary op l r) = V.OpII (dispatch'' op) (autoToVSat' l) (autoToVSat' r)

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

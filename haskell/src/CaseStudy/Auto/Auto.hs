module CaseStudy.Auto.Auto where

import Data.Aeson
import Data.Text

import VProp.Types
import VProp.Core

import CaseStudy.Auto.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ByteString.Lazy.Internal

-- import Api

-- | A context represents an evolution context which are temporal bounds
-- represented as integers
type Context = (Integer, Integer)

-- | The auto type encodes the context of the automotive encoding, and the
-- constraints that range over the features in the automotive model
data Auto = Auto { contexts :: Context
                 , constraints :: [Text]}
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

x :: Data.ByteString.Lazy.Internal.ByteString
x = "{\"attributes\":[],\"contexts\":[{\"id\":\"context[_evolution-context]\",\"min\":-1,\"max\":2}],\"configuration\":{\"selected_features\":[],\"attribute_values\":[],\"context_values\":[{\"id\":\"context[_evolution-context]\",\"value\":-1}]},\"constraints\":[\"feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1\",\"feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1 impl (feature[_509c475f-5491-4719-8644-98bb317e9995] = 1 and feature[_3b96f751-9f07-4fd1-b2ef-6d4b9328fc8c] = 1 and feature[_8bf25ba9-9b19-416f-baf6-f2eb58b573fe] = 1)\",\"(feature[_509c475f-5491-4719-8644-98bb317e9995] = 1 or feature[_3b96f751-9f07-4fd1-b2ef-6d4b9328fc8c] = 1 or feature[_8bf25ba9-9b19-416f-baf6-f2eb58b573fe] = 1) impl feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1\",\"feature[_509c475f-5491-4719-8644-98bb317e9995] = 1 impl feature[_a63857d6-2617-4a9d-911d-1f59aa423d49] = 1\",\"feature[_a63857d6-2617-4a9d-911d-1f59aa423d49] = 1 impl feature[_509c475f-5491-4719-8644-98bb317e9995] = 1\",\"(context[_evolution-context] < 0) impl (feature[_a63857d6-2617-4a9d-911d-1f59aa423d49] = 1 impl (feature[_b493ac45-c0a8-4c93-a622-98a3d5a981a8] = 1\"]}"

a :: Text
a = "feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1 impl (feature[_509c475f-5491-4719-8644-98bb317e9995] = 1 and feature[_3b96f751-9f07-4fd1-b2ef-6d4b9328fc8c] = 1 and feature[_8bf25ba9-9b19-416f-baf6-f2eb58b573fe] = 1)"

b :: Text
b = "feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1 and feature[_509c475f-5491-4719-8644-98bb317e9995] = 1"

c :: Text
c = "feature[_509c475f-5491-4719-8644-98bb317e9995] = 1"

d :: Text
d = "(context[_evolution-context] < 0) impl (feature[_a63857d6-2617-4a9d-911d-1f59aa423d49] = 1 impl (feature[_b493ac45-c0a8-4c93-a622-98a3d5a981a8] = 1 and feature[_0e8e9baa-56f5-48d3-93dd-1f4db1d546d4] = 1 and feature[_6aa42cc5-013e-4b44-b75b-09c55c90e8a7] = 1 and feature[_1f3c4f9e-fc8a-44bb-91de-33fdf84ad535] = 1 and feature[_84928c30-724e-4e73-b9fb-733518d0e3c6] = 1 and feature[_53e5b7e7-7ae7-44cd-a740-8d993d7eb86a] = 1 and feature[_e310f7d9-7ad6-4732-8fb2-ae3e24a9a7e5] = 1 and feature[_c4ca2f88-0407-40b0-910d-241327bde72c] = 1 and feature[_2ad480a4-eb1b-4846-bb97-00595e23faf3] = 1 and feature[_375ddff0-aa5a-44d8-8545-d5fb62fd53e8] = 1 and feature[_47fca63b-434a-46d7-9c3c-8b77eeb86d5b] = 1 and feature[_a10b3970-245e-4f43-869f-0b0f48f9390b] = 1 and feature[_67419fde-def3-4dc9-8124-5a7e76600e33] = 1 and feature[_c7f5b43a-d84b-486b-bafa-4eabc1020ba1] = 1 and feature[_9fe16054-55b2-4482-b6ec-d10ffc9e4e6e] = 1 and feature[_e6460757-4ba6-40e9-a348-c236e34c6a0c] = 1 and feature[_30b661fe-1318-4dab-ad0d-8fbfdee925ee] = 1 and feature[_dc6635d4-7e7c-4b67-9e4e-84a0cfc0a9ac] = 1 and feature[_934798c5-75f7-4894-8202-8b28577cca22] = 1 and feature[_4cadf655-6436-45ef-8d1a-5275b2b71fe9] = 1 and feature[_85ce67eb-cbd6-4256-b6a6-3558abf370f0] = 1 and feature[_33125359-6c3d-4953-8448-9a9b49f38fd4] = 1 and feature[_32e4bd1a-f62f-4f71-a3b6-6cb0ff8e2deb] = 1 and feature[_3ce915e1-c65e-4b1e-bd3e-57f2f517ad40] = 1 and feature[_9a75ad02-8c39-4d4c-8232-5d49f146dbd9] = 1 and feature[_52d44fbd-d580-4dcc-bed9-8fb1c960b34d] = 1 and feature[_53c1023e-584e-42cf-998d-5a6ff50dc237] = 1 and feature[_cf79eb43-6421-43d2-818b-4fefff12ff3d] = 1 and feature[_47a3155b-fda9-45f0-8fae-a557ead35d06] = 1 and feature[_ca068bac-3dd8-4503-9506-db7198e9ae7d] = 1 and feature[_b47c8cb0-0b60-46dd-b2c5-5853a5fc647d] = 1)"

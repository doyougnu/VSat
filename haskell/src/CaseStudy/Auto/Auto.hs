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
b = "(feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1 impl feature[_509c475f-5491-4719-8644-98bb317e9995] = 1)"

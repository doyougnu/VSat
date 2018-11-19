module CaseStudy.Auto.Auto where

import Data.Aeson
import Data.Text

import VProp.Types
import VProp.Core


import Data.ByteString.Lazy.Internal
import Debug.Trace (trace)

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
x = "{\"attributes\":[],\"contexts\":[{\"id\":\"context[_evolution-context]\",\"min\":-1,\"max\":2}],\"configuration\":{\"selected_features\":[],\"attribute_values\":[],\"context_values\":[{\"id\":\"context[_evolution-context]\",\"value\":-1}]},\"constraints\":[\"feature[_131c943a-ac13-4b7e-b840-ba2437f026fc] = 1\"]}"

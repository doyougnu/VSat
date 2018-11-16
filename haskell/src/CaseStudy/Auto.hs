module Auto where

import Data.Aeson

import VProp.Types
import Api

-- | A context represents an evolution context which are temporal bounds
-- represented as integers
type Context = (Integer, Integer)

-- | The auto type encodes the context of the automotive encoding, and the
-- constraints that range over the features in the automotive model
data Auto = Auto { contexts :: Context
                 , constraints :: VProp String String}

instance FromJSON (VProp String String) where
  parseJSON = undefined

module Server where

import Web.Spock hiding (Var)
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson hiding (json)

import Api
import Json
import V
import Config
import VProp.Gen
import VProp.Types

-- * Api is the type for the spock server. I leave each config parameter as Unit
-- * because I do not need a backend only a server daemon
type Api = SpockM () () ()

-- * ApiAction represents Spock actions, these are handlers
type ApiAction a = SpockAction () () () a

data Request a b = Request { solver :: String
                           -- , opts   :: [VProp a b -> VProp a b]
                           , seed   :: Integer
                           , getProp :: VProp a b}
  deriving (Generic)

instance FromJSONKey Dim
instance (FromJSON a, FromJSON b) => FromJSON (Request a b)

-- satHandler :: Api [V String (Maybe SatResult)]
-- satHandler :: SpockCtxM ctx conn sess st ()
-- satHandler = do post "sat" $ do
--                   prop <- liftIO $ (genVProp :: IO (VProp Var Var))
--                   res <- liftIO $ sat (bimap show show prop)
--                   json res

-- satHandler :: SpockCtxM ctx conn sess st ()
satHandler = do post "sat" $ do
                  req <- jsonBody' :: ApiAction (Request Var Var)
                  let prop = getProp req
                  res <- liftIO $ sat (bimap show show prop)
                  json res

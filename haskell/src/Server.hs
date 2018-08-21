module Server where

import Web.Spock hiding (Var)
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson hiding (json)
import Data.Maybe (maybe)

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

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

data Request a b = Request { settings :: Maybe Settings
                           , proposition :: VProp a b
                           }
  deriving (Generic,Show)

instance FromJSONKey Dim
instance (FromJSON a, FromJSON b) => FromJSON (Request a b)
instance (ToJSON a, ToJSON b) => ToJSON (Request a b)


app :: Api ()
app = do post "sat" satHandler
         post "prove" proveHandler

satHandler = do
  req <- jsonBody' :: ApiAction (Request String String)
  let prop = proposition req
      sets = maybe defSettings id (settings req)
      conf = toConf sets
  res <- liftIO $ satWith conf prop
  json res

proveHandler = do
  req <- jsonBody' :: ApiAction (Request Var Var)
  let prop = proposition req
      sets = maybe defSettings id (settings req)
      conf = toConf sets
  res <- liftIO $ proveWith conf (bimap show show prop)
  json res

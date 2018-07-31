module Server where

import Web.Spock hiding (Var)
import Web.Spock.Config
import Control.Monad.IO.Class (liftIO)

import Api
import Json
import V
import VProp.Gen
import VProp.Types

-- * Api is the type for the spock server. I leave each config parameter as Unit
-- * because I do not need a backend only a server daemon
type Api = SpockM () () ()

-- * ApiAction represents Spock actions, these are handlers
type ApiAction a = SpockAction () () () a

-- satHandler :: Api [V String (Maybe SatResult)]
satHandler :: SpockCtxM ctx conn sess st ()
satHandler = do get "sat" $ do
                  prop <- liftIO $ (genVProp :: IO (VProp Var Var))
                  res <- liftIO $ sat [] (bimap show show prop)
                  json res

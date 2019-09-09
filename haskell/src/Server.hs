module Server where

import Web.Spock hiding (Var)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson hiding (json)
import Data.Maybe (fromMaybe)
import Network.Wai.Middleware.RequestLogger
import Data.Text (Text)

import Data.ByteString.Lazy (appendFile)
import Data.ByteString.Char8 (pack)

import Data.Csv as C
import Prelude hiding (writeFile, appendFile)

import Control.Concurrent (forkIO)
import System.CPUTime
import System.Timeout
import Data.Time.Clock

import Api
import Json
import Opts
import Config
import VProp.Types
import VProp.Core
import SAT

-- * Api is the type for the spock server. I leave each config parameter as Unit
-- * because I do not need a backend only a server daemon
type Api = SpockM () () ()

-- * ApiAction represents Spock actions, these are handlers
type ApiAction a = SpockAction () () () a

data Request d a b = Request { settings :: Maybe Settings
                             , proposition :: VProp d a b
                             }
  deriving (Generic,Show)

instance (FromJSON a, FromJSONKey a) => FromJSONKey (Dim a)
instance (FromJSON d, FromJSON a, FromJSON b) => FromJSON (Request d a b)
instance (ToJSON d, ToJSON a, ToJSON b) => ToJSON (Request d a b)

app :: Api ()
app = do
  middleware logStdout
  post "satWith" satWithHandler
  -- post "proveWith" proveWithHandler
  post "sat" satHandler
  -- post "prove" proveHandler

logfile :: FilePath
logfile = "timing_desc_data.csv"

-- TODO cleanup these types and refactor out the logging to middleware
satWithHandler :: ActionCtxT () (WebStateM () () ()) b
satWithHandler = do
  req <- jsonBody' :: ApiAction (Request Var Text Text)
  let prop = proposition req
      sets = fromMaybe defSettings (settings req)
      conf = toConf sets
  (runtime, res) <- liftIO . timeProc . satWith conf $ prop
  _ <- liftIO . forkIO $ logData prop sets runtime logfile
  json res

-- proveWithHandler :: ActionCtxT () (WebStateM () () ()) b
-- proveWithHandler = do
--   req <- jsonBody' :: ApiAction (Request Var Var Var)
--   let prop = proposition req
--       sets = maybe defSettings id (settings req)
--       conf = toConf sets
--   (runtime, res) <- liftIO . timeProc . proveWith conf $ prop
--   _ <- liftIO . forkIO $ logData prop sets runtime logfile
--   json res

satHandler :: ActionCtxT () (WebStateM () () ()) b
satHandler = do
  req <- jsonBody' :: ApiAction (Request Var Text Text)
  let prop = proposition req
  (runtime, res) <- liftIO . timeProc . sat $ prop
  _ <- liftIO . forkIO $ logData prop defSettings runtime logfile
  json res

-- proveHandler :: ActionCtxT () (WebStateM () () ()) b
-- proveHandler = do
--   req <- jsonBody' :: ApiAction (Request Var Var Var)
--   let prop = proposition req
--   (runtime, res) <- liftIO . timeProc . prove $ prop
--   _ <- liftIO . forkIO $ logData prop defSettings runtime logfile
--   json res

-- * Time logging functions

-- | Required field namings for cassava csv library
data RunData = RunData { utc_            :: !UTCTime
                       , settings_       :: !Settings
                       , runTime_        :: !Double
                       , numTerms_       :: !Integer
                       , numChc_         :: !Integer
                       , numPlain_       :: !Integer
                       , numSharedDims_  :: !Integer
                       , numSharedPlain_ :: !Integer
                       , maxShared_      :: !Integer
                       } deriving (Generic, Show)

instance C.ToField UTCTime  where toField = pack . show
instance C.ToField Opts     where toField = pack . show
instance C.ToField Solver   where toField = pack . show
instance C.ToField Settings where toField = pack . show
instance C.ToField [Opts]   where toField = mconcat . fmap (pack . show)
instance C.ToRecord RunData
instance C.ToRecord Settings
instance C.ToRecord Opts

logData :: VProp Var Text Text -> Settings -> Double -> FilePath -> IO ()
logData prop sets runTime fn =
  do time <- getCurrentTime
     let row = RunData time sets runTime s c p sd sp ms
     appendFile fn . C.encode $ pure row
  where descriptorsFs = [ numTerms
                        , numChc
                        , numPlain
                        , numSharedDims
                        , numSharedPlain
                        , toInteger . maxShared
                        ]
        [s,c,p,sd,sp,ms] = descriptorsFs <*> pure prop

timeProc :: IO b -> IO (Double, b)
timeProc !a = do
  start <- getCPUTime
  v <- a
  end' <- timeout 300000000 (v `seq` getCPUTime)
  let end = maybe (300 * 10^12) id end'
      diff = (fromIntegral (end - start)) / (10 ^ 12)
  return (diff,  v)

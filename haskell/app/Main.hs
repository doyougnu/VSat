module Main where

import Web.Spock hiding (Var)
import Web.Spock.Config

import VProp.Types
import VProp.Gen
import Data.Aeson.Encode.Pretty
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import Server

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

module Main where

import Web.Spock
import Web.Spock.Config

import VProp.Types as T
import VProp.Gen
import Server

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

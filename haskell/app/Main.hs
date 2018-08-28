module Main where

import Web.Spock hiding (Var)
import Web.Spock.Config
import System.Environment (getEnv)

-- Keeping unused imports for repl experience, see docs
import VProp.Types
import VProp.Gen
import Data.Aeson.Encode.Pretty
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import Server

print = B.putStrLn

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  port' <- read <$> getEnv "TEST"
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  putStrLn $ "running on port: " ++ show (port' :: Integer)
  runSpock port (spock spockCfg app)

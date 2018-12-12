import qualified Data.ByteString as BS (readFile)
import Text.Megaparsec (parse)
import Data.Either (rights)
import Data.Text (unpack)
import Data.Bifunctor (bimap)
import Data.Map.Internal.Debug (showTree)
import Control.Arrow (first, second)
import System.IO

import CaseStudy.Auto.Auto
import CaseStudy.Auto.Parser (langParser)
import VProp.Core
import VProp.Types
import Api
import V

import Data.Aeson (decodeStrict)

autoFile :: FilePath
autoFile = "src/CaseStudy/Auto/Automotive02_merged_evolution_history_integer.json"

-- main :: IO (V String (Maybe ThmResult))
main = do
  jsn <- BS.readFile autoFile
  let (Just auto) = decodeStrict jsn :: Maybe Auto
      cs = constraints auto
      ps' = parse langParser "" <$> cs
      ps = rights ps'
      prop = bimap unpack unpack <$> autoToVSat $ conjoin ps
  -- print $ take 5 $ autoToVSat <$> ps
  res <- sat . flatten $ prop
  writeFile "testoutput" (show res)
  -- print $ VProp.Core.dimensions $ flatten prop
  -- print res
  -- return res
  -- mapM (putStrLn . show) $ (second flatten) <$> (zip [1..] $ prop)
  -- print $ flatten . last . take 10 . drop 1210 $ prop
  -- putStrLn . showTree . fst . snd $ prop'

import           Control.Arrow           (first, second)
import           Data.Bifunctor          (bimap)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (rights)
import           Data.Map.Internal.Debug (showTree)
import           Data.Text               (unpack)
import           System.IO
import           Text.Megaparsec         (parse)


import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           V
import           VProp.Core
import           VProp.Types

import           Data.Aeson              (decodeStrict)

autoFile :: FilePath
autoFile = "src/CaseStudy/Auto/Automotive02_merged_evolution_history_integer.json"


smAutoFile :: FilePath
smAutoFile = "src/CaseStudy/Auto/vsat_small_example.json"

-- main :: IO (V String (Maybe ThmResult))
main = do
  jsn <- BS.readFile smAutoFile
  let (Just auto) = decodeStrict jsn :: Maybe Auto
      cs = constraints auto
      ps' = parse langParser "" <$> cs
      ps = rights ps'
      prop = bimap unpack unpack <$> (naiveEncode . nestChoices . autoToVSat) $ conjoin ps
  -- print $ take 5 $ autoToVSat <$> ps
  res <- sat . flatten $ prop
  putStrLn "\n\n ----------------- \n\n"
  traverse print $ res
  -- writeFile "testoutput" (show res)
  -- print $ VProp.Core.dimensions $ flatten prop
  -- print res
  -- return res
  -- mapM (putStrLn . show) $ (second flatten) <$> (zip [1..] $ prop)
  -- print $ flatten . last . take 10 . drop 1210 $ prop
  -- putStrLn . showTree . fst . snd $ prop'

import           Control.Arrow           (first, second)
import           Data.Aeson              (decodeStrict)
import           Data.Bifunctor          (bimap)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (rights, lefts)
import           Data.Map.Internal.Debug (showTree)
import           Data.Text               (unpack)
import           System.IO
import           Text.Megaparsec         (parse)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Parser   (langParser)
import           Config                  (defConf, emptyConf)
import           Opts
import           Run                     (runAD, runBF)
import           V
import           VProp.Core
import           VProp.Types

-- | a large dataset of queries
autoFile :: FilePath
autoFile = "src/CaseStudy/Auto/Automotive02_merged_evolution_history_integer.json"

-- | a different file that represents a possible json
smAutoFile :: FilePath
smAutoFile = "src/CaseStudy/Auto/vsat_small_example.json"

-- | a chunk of the large autoFile above
chAutoFile :: FilePath
chAutoFile = "src/CaseStudy/Auto/vsat_small_chunk.json"

-- main :: IO (V String (Maybe ThmResult))
main = do
  jsn <- BS.readFile smAutoFile
  let (Just auto) = decodeStrict jsn :: Maybe Auto
      cs = constraints auto
      -- ps' = parse langParser "" <$> (drop 15 . take 20 $ cs)
      ps' = parse langParser "" <$> cs
      ps = rights ps'
      bs = lefts ps'
      prop = (bimap unpack unpack . naiveEncode . nestChoices . flatten . autoToVSat) <$> ps
  -- print $ take 1 cs
  putStrLn "\n\n ----------------- \n\n"

  -- print (conjoin' prop)
  -- print $ take 5 $ autoToVSat <$> ps

  res <- runBF emptyConf $ conjoin' prop
  res' <- satWith emptyConf $ conjoin' prop
  res'' <- runAD emptyConf $ conjoin' prop
  -- print res'
  -- writeFile "rights" (show $ prop)
  -- writeFile "lefts" (foldMap show bs)
  writeFile "testoutputBF" (show res)
  writeFile "testoutputSAT" (show res')
  writeFile "testoutputAD" (show res'')
  -- print $ VProp.Core.dimensions $ flatten prop
  -- print res
  -- return res
  -- mapM (putStrLn . show) $ (second flatten) <$> (zip [1..] $ prop)
  -- print $ flatten . last . take 10 . drop 1210 $ prop
  -- putStrLn . showTree . fst . snd $ prop'
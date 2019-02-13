import           Control.Arrow           (first, second)
import           Data.Aeson              (decodeStrict)
import           Data.Bifunctor          (bimap)
import qualified Data.ByteString         as BS (readFile)
import           Data.Either             (lefts, rights)
import           Data.Map.Internal.Debug (showTree)
import           Data.Text               (unpack, pack)
import qualified Data.SBV                as S (sat)
import           System.IO
import           Data.List               (sort)
import           Text.Megaparsec         (parse)

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Run
import           CaseStudy.Auto.Parser   (langParser)
import           Config                  (defConf, emptyConf)
import           Opts
import           Run                     (runAD, runBF)
import           Utils
import           VProp.Core
import           VProp.Types
import           VProp.SBV               (toPredicate)

-- | a large dataset of queries
-- autoFile :: FilePath
-- autoFile = "bench/AutoBench/Automotive02_merged_evolution_history_integer.json"

autoFileBool :: FilePath
autoFileBool = "bench/AutoBench/Automotive02_merged_evolution_history_boolean.json"

-- | a different file that represents a possible json
smAutoFile :: FilePath
smAutoFile = "bench/AutoBench/vsat_small_example.json"

-- | a chunk of the large autoFile above
chAutoFile :: FilePath
chAutoFile = "bench/AutoBench/vsat_small_chunk.json"

-- main :: IO (V String (Maybe ThmResult))
main = do
  jsn <- BS.readFile autoFileBool
  let (Just auto) = decodeStrict jsn :: Maybe Auto
      cs = constraints auto -- looks like 4298/4299 are the culprits
      -- ps' = parse langParser "" <$> (drop 15 . take 20 $ cs)
      ps' = parse langParser "" <$> cs
      ps = rights ps'
      bs = lefts ps'
      prop = (naiveEncode . nestChoices . autoToVSat) $ autoAndJoin ps
      prop' = (naiveEncode . nestChoices . autoToVSat) <$> ps
  -- print $ take 1 cs
  putStrLn "\n\n ----------------- \n\n"
  -- mapM_ print (fmap idEncode (take 1 $ drop 1 ps) )
  putStrLn "\n\n ----------------- \n\n"
  -- mapM_ print (sort prop)

  -- print (conjoin' prop)
  -- print $ take 5 $ autoToVSat <$> ps

  -- conjoining and then naive sat call is 84 seconds
  -- (incTime, incRes) <- time $ (S.sat . toPredicate) $ (autoAndJoin (fmap idEncode ps))
  -- not conjoining and fmapping is 79 seconds, probably within the noise
  -- (incTime, incRes) <- time $ mapM (S.sat . toPredicate) (fmap idEncode ps)
  -- (bfTime, res) <- time $ bfWith emptyConf $ prop
  (satTime, res') <- time $! satWith emptyConf $ prop
  -- (adTime, res'') <- time $! adWith emptyConf id $ prop
  -- putStrLn ("Incremental Solve [s]: " ++ show incTime ++ "\n")
  -- putStrLn ("Brute Force Time [s]: " ++ show bfTime ++ "\n")
  putStrLn ("VSAT Time        [s]: " ++ show satTime ++ "\n")
  -- putStrLn ("And Decomp Time  [s]: " ++ show adTime ++ "\n")
  -- writeFile "rights" (show $ prop)
  -- writeFile "lefts" (foldMap show bs)
  -- writeFile "testoutputInc" (show incRes)
  -- writeFile "testoutputBF" (show res)
  -- writeFile "testoutputSAT" (show res')
  -- writeFile "testoutputAD" (show res'')
  -- print $ VProp.Core.dimensions $ flatten prop
  -- print res
  -- return res
  -- mapM (putStrLn . show) $ (second flatten) <$> (zip [1..] $ prop)
  -- print $ flatten . last . take 10 . drop 1210 $ prop
  -- putStrLn . showTree . fst . snd $ prop'

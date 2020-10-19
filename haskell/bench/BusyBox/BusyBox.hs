module BusyBox where


import           Control.Arrow           (first, second)
import           Gauge
import           Data.Aeson              (decodeStrict, encodeFile)
import           Control.Monad           (replicateM, foldM, liftM2)
import           Data.Bifunctor          (bimap)
import           Data.Bitraversable      (bimapM)
import           Data.Either             (lefts, rights)
import           Data.Foldable           (foldr')
import qualified Data.List               as L
import qualified Data.Map.Strict         as M
import qualified Data.SBV                as S
import qualified Data.SBV.Control        as SC
import qualified Data.SBV.Internals      as SI
import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           System.IO
import           Text.Megaparsec         (parse)
import Numeric
import Data.Char (intToDigit)
import qualified System.Directory        as D
import qualified Control.Exception       as E

import           Api
import           CaseStudy.Auto.Auto
import           CaseStudy.BusyBox.Parser (langParser)
import           Config
import           Opts
import           Json
import           Run                     (runAD, runBF)
import           Result
import           Utils
import           VProp.Core
import           VProp.SBV               (toPredicate)
import           VProp.Types

import           Core


newtype Directory = Directory { unDir :: FilePath }
  deriving (Eq, Show)

-- some constants
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ "/" ++ b

isDirPlain :: Directory -> Bool
isDirPlain = (==) plainDir

plainDir = Directory "plain"
prefix = "SAT_problems_"
suffix = ".txt"
mkFileConst a = prefix <> a <> suffix

featureModel   = mkFileConst "FEATURE_MODEL"
parseProblems  = mkFileConst "PARSING"
lexingProblems = mkFileConst "LEXING"
tcProblems     = mkFileConst "TYPE_CHECKING"
noModeProblems     = mkFileConst "NO_MODE"

newtype Analysis = Analysis { getAnalysis :: M.Map QueryMode [ReadableProp T.Text] }
  deriving (Eq,Ord,Show)

instance Semigroup Analysis where (getAnalysis -> a) <> (getAnalysis -> b) = Analysis $! M.unionWith (<>) a b
instance Monoid Analysis where mempty = Analysis M.empty

data QueryMode = Lexing
               | Parsing
               | TypeChecking
               | NoMode
               | FeatureModel
               deriving (Eq,Ord,Show)

-- | a record to store an analysis. I've hardcoded these particular to busybox,
-- if the analysis done in type chef changes these will also have to change

dataFiles :: IO [Directory]
dataFiles = fmap (Directory . (home </>)) <$> D.listDirectory home
  where home = "bench/BusyBox/sat_queries"

readPropFile :: FilePath -> IO [ReadableProp T.Text]
readPropFile f = do txtProblems <- T.lines <$> TIO.readFile f
                    let problems' = parse langParser "" <$> txtProblems
                    return $ rights problems'

readFM :: Directory -> IO (ReadableProp T.Text)
readFM (unDir -> d) = go `E.catch` \e -> print (e :: E.IOException) >> return true
  where go = readPropFile (d </> featureModel) >>= \c ->
          return $
          case c of
            [] -> true     -- then we were in the plain directory
            xs -> head xs  -- then we found the feature model

readParseProblems :: Directory -> IO [ReadableProp T.Text]
readParseProblems (unDir -> d) = readPropFile (d </> parseProblems)
                    `E.catch` \e -> print (e :: E.IOException) >> return []

readLexingProblems :: Directory -> IO [ReadableProp T.Text]
readLexingProblems (unDir -> d) = readPropFile (d </> lexingProblems)
                    `E.catch` \e -> print (e :: E.IOException) >> return []

readTcProblems :: Directory -> IO [ReadableProp T.Text]
readTcProblems (unDir -> d) = readPropFile (d </> tcProblems)
                    `E.catch` \e -> print (e :: E.IOException) >> return []

readNoModeProblems :: Directory -> IO [ReadableProp T.Text]
readNoModeProblems (unDir -> d) = readPropFile (d </> noModeProblems)
                     `E.catch` \e -> print (e :: E.IOException) >> return []

-- | because we set the feature model mode initially all queries will be
-- captured as a feature model if no feature model exists. We combine this with
-- the no mode queries which is the only other mode that plain queries will be
-- found in.
handlePlain :: Directory -> IO [ReadableProp T.Text]
handlePlain dir = do fs <- fm
                     ns <- nm
                     (return (fs ++ ns)) `E.catch`
                       \e -> print (e :: E.IOException) >> return []
  where
    d  = unDir dir
    fm = readPropFile (d </> featureModel)
    nm = readNoModeProblems dir


mkAnalysis :: Directory -> IO Analysis
mkAnalysis d = do putStrLn $ "Reading: " ++ (unDir d)
                  if (isDirPlain d)

                    then do qs <- handlePlain d
                            return $ Analysis $ M.singleton NoMode qs

                    else do fm <- M.singleton FeatureModel <$> readFM             d
                            pp <- M.singleton Parsing      <$> readParseProblems  d
                            lp <- M.singleton Lexing       <$> readLexingProblems d
                            tp <- M.singleton TypeChecking <$> readTcProblems     d
                            np <- M.singleton NoMode       <$> readNoModeProblems d
                            return $ Analysis $ mconcat [pp,lp,tp,np]


getProblems :: IO [Analysis]
getProblems = dataFiles >>= mapM mkAnalysis

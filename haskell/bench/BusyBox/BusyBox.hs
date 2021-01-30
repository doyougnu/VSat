{-# LANGUAGE DeriveAnyClass #-}

module BusyBox where


import           Control.Arrow           (first, second)
import           Control.DeepSeq         (force,NFData)
import           GHC.Generics            (Generic)
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
  deriving (Eq, Show,Generic,NFData)

-- some constants
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ "/" ++ b

isDirPlain :: Directory -> Bool
isDirPlain = (==) plainDir

plainDir = Directory "plain"
prefix = "SAT_problems_"
suffix = ".txt"
mkFileConst a = prefix <> a <> suffix

featureModelLbl   = "FEATURE_MODEL.txt"
parseProblems  = mkFileConst "PARSING"
lexingProblems = mkFileConst "LEXING"
tcProblems     = mkFileConst "TYPE_CHECKING"
noModeProblems     = mkFileConst "NO_MODE"

newtype Analysis a b = Analysis { getAnalysis :: M.Map QueryMode [VProp T.Text a b] }
  deriving (Eq,Ord,Show,Generic,NFData)

instance Semigroup (Analysis a b) where (getAnalysis -> a) <> (getAnalysis -> b) = Analysis $! M.unionWith (<>) a b
instance Monoid (Analysis a b) where mempty = Analysis M.empty

data QueryMode = NoMode
               | FeatureModel
               | Lexing
               | Parsing
               | TypeChecking
               deriving (Eq,Ord,Show,Generic,NFData)

-- | don't feel like making the correct semigorup and monoid instances for maybe
-- here
get :: QueryMode -> Analysis a b -> [VProp T.Text a b]
get m ( getAnalysis -> a) = case m `M.lookup` a of
                              Nothing -> mempty
                              Just xs -> xs


featureModel :: Analysis a b -> VProp T.Text a b
featureModel a = case get FeatureModel a of
                   [] -> true
                   xs -> head xs

lexing :: Analysis a b -> [VProp T.Text a b]
lexing = get Lexing

parsing :: Analysis a b -> [VProp T.Text a b]
parsing = get Parsing

typeChecking :: Analysis a b -> [VProp T.Text a b]
typeChecking = get TypeChecking

noMode :: Analysis a b -> [VProp T.Text a b]
noMode = get NoMode

dataFiles :: IO [Directory]
dataFiles = fmap (Directory . (home </>)) <$> D.listDirectory home
  where home = "bench/BusyBox/sat_queries"
  -- where home = "/home/doyougnu/research/TypeChef-BusyboxAnalysis/sat_queries"

readPropFile :: FilePath -> IO [ReadableProp T.Text]
readPropFile f = do txtProblems <- T.lines <$> TIO.readFile f
                    let !problems' = parse langParser "" <$> txtProblems
                    return $! rights problems'

readFM :: Directory -> IO (ReadableProp T.Text)
readFM (unDir -> d) = go `E.catch` \e -> print (e :: E.IOException) >> return true
  where go = readPropFile (d </> featureModelLbl) >>= \c ->
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
    fm = readPropFile (d </> featureModelLbl)
    nm = readNoModeProblems dir

findPlain :: [Analysis a b] -> Analysis a b
findPlain xs = go
  where isAnaPlain (getAnalysis -> a) = M.member FeatureModel a &&
                                        M.member NoMode a &&
                                        M.notMember TypeChecking a &&
                                        M.notMember Lexing a &&
                                        M.notMember Parsing a
        go = case filter isAnaPlain xs of
               []    -> mempty
               (x:_) -> x

mkAnalysis :: Directory -> IO (Analysis Readable Readable)
mkAnalysis d = do putStrLn $ "Reading: " ++ (unDir d)
                  if (isDirPlain d)

                    then do !qs <- handlePlain d
                            return $! Analysis $! M.singleton NoMode qs

                    else do !fm <- M.singleton FeatureModel <$> readFM             d
                            putStrLn "Got fm"
                            !pp <- M.singleton Parsing      <$> readParseProblems  d
                            putStrLn "Got parsing"
                            !lp <- M.singleton Lexing       <$> readLexingProblems d
                            putStrLn "Got lexing"
                            !tp <- M.singleton TypeChecking <$> readTcProblems     d
                            putStrLn "Got type checking"
                            !np <- M.singleton NoMode       <$> readNoModeProblems d
                            putStrLn "Got no mode"
                            return $ Analysis $! mconcat [pp,lp,tp,np]


base :: Integer -> Integer -> Integer
base b = ceiling . logBase (fromInteger b) . fromInteger

hole :: ReadableProp T.Text
hole = bRef "__"

prop :: Maybe Int -> [ReadableProp T.Text] -> ReadableProp T.Text
prop Nothing xs  = prop (Just 0) xs
prop (Just !i) xs = outer i xs
  where
    outer !i [x] = x
    outer !i xs  = outer (succ i) (inner (T.pack $ show i) xs)


    inner :: T.Text -> [ReadableProp T.Text] -> [ReadableProp T.Text]
    inner _ [] = []
    inner _ [x] = [x]
    inner !d (x:y:xs) = ChcB (Dim d) x y : inner d xs

propOpts :: Maybe Int -> [ReadableProp T.Text] -> ReadableProp T.Text
propOpts Nothing xs  = atomize $ propOpts (Just 0) xs
propOpts (Just !i) xs = atomize $ outer i xs
  where
    outer _ [x] = x
    outer !i ys  = outer (succ i) (atomize <$> inner (T.pack $ show i) ys)


    inner :: T.Text -> [ReadableProp T.Text] -> [ReadableProp T.Text]
    inner _ [] = []
    inner _ [x] = [x]
    inner !d (x:y:ys) = ChcB (Dim d) x y : inner d ys

getProblems :: IO [Analysis Readable Readable]
getProblems = dataFiles >>= mapM (force mkAnalysis) -- . take 20

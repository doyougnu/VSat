module Api ( sat
           , satWith
           , satWithConf
           , bfWith
           , bfWithConf
           , bf
           , adWith
           , ad
           , DimProp(..)
           , toDimProp
           , pOnVWithConf
           , genConfigPool
           , genConfigPool'
           , vOnPWithConf
           , vOnPWith
           ) where

import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)
import qualified Data.SBV           as S
import           Data.SBV.Internals (cvToBool)
import qualified Data.Set           as Set
import           Data.String        (IsString(..))
import Data.Text (Text)

import           VProp.Core
import           VProp.SBV
import           VProp.Types

import           Config             (defConf, ReadableSMTConf)

import           Result
import           Run
import           SAT
import           Utils              (fst')

-- | a newtype wrapper to denote that this proposition can only have dimensions
-- as variables
newtype DimProp d = DimProp {getDimProp :: VProp d (Dim String) (Dim String)}
  deriving (Boolean,Show)

toDimProp :: VProp d String String -> Maybe (DimProp d)
toDimProp = Just . DimProp . trimap id f f
  where f = Dim

instance (Show d, Ord d) =>
  SAT (DimProp d) where toPredicate = symbolicPropExpr'

genConfigPool' :: (Resultable d) => Maybe (DimProp d) -> IO [Config d]
genConfigPool' Nothing  = return mempty
genConfigPool' (Just p) =
  do
    S.AllSatResult (_,_,_,allRes) <- S.allSat $ toPredicate p
    let resMaps = S.getModelDictionary <$> allRes
    return $!
      M.foldMapWithKey (\k a -> M.singleton (Dim $ fromString k) (cvToBool a)) <$> resMaps

genConfigPool :: (Resultable d) => VProp d String String -> IO [Config d]
genConfigPool = genConfigPool' . toDimProp

-- | Generate a symbolic predicate for a feature expression.
symbolicPropExpr' :: (Ord d,Show d) => DimProp d -> S.Predicate
symbolicPropExpr' e' = do
    let e = trimap id dimName dimName $ getDimProp e'
        vs = Set.toList (bvars e)
        ds = Set.toList (dimensions e)
        isType = Set.toList (ivarsWithType e)

        helper (RefD, d) = sequence $ (d, SD <$> S.sDouble d)
        helper (RefI, i) = sequence $ (i, SI <$> S.sInt64 i)

    syms  <- fmap (M.fromList . zip vs) (S.sBools vs)
    dims  <- fmap (M.fromList . zip ds) (S.sBools (map (show . dimName) ds))
    isyms <- M.fromList <$> traverse helper isType
    let look f  = fromMaybe err  (M.lookup f syms)
        lookd d = fromMaybe errd (M.lookup d dims)
        looki i = fromMaybe erri (M.lookup i isyms)
    return (evalPropExpr lookd looki look e)
  where err = error "symbolicPropExpr: Internal error, no symbol found."
        errd = error "symbolicPropExpr: Internal error, no dimension found."
        erri = error "symbolicPropExpr: Internal error, no int symbol found."


-- | Run VSMT and return variable bindings
sat :: (Show d, Resultable d, SAT (ReadableProp d)) =>
  ReadableProp d -> IO (Result d)
sat = satWith defConf

satWith :: (Show d, Resultable d)
  => ReadableSMTConf d -> ReadableProp d -> IO (Result d)
satWith = satWithConf Nothing

satWithConf :: (Show d, Resultable d)
  => Maybe (DimProp d) -> ReadableSMTConf d -> ReadableProp d -> IO (Result d)
satWithConf Nothing          conf prop = fst' <$> runVSMT mempty conf prop
satWithConf dimConfig conf prop =
  do
    configPool <- genConfigPool' dimConfig
    -- mapM_ (putStrLn . show) configPool
    -- putStrLn . show $ (length configPool)
    fst' <$> runVSMT configPool conf prop


bfWithConf :: (Show d, Resultable d, SAT (ReadableProp d))
  => Maybe (DimProp d) -> ReadableSMTConf d -> ReadableProp d -> IO (Result d)
bfWithConf Nothing          conf prop = runBF mempty conf prop
bfWithConf dimConfig conf prop =
  do
    configPool <- genConfigPool' dimConfig
    -- mapM_ (putStrLn . show) configPool
    -- putStrLn . show $ (length configPool)
    runBF configPool conf prop

vOnPWithConf :: (Show d, Resultable d, SAT (ReadableProp d))
  => Maybe (DimProp d) -> ReadableSMTConf d -> ReadableProp d -> IO (Result d)
vOnPWithConf Nothing          conf prop = runVonP mempty conf prop
vOnPWithConf dimConfig conf prop =
  do
    configPool <- genConfigPool' dimConfig
    -- mapM_ (putStrLn . show) configPool
    -- putStrLn . show $ (length configPool)
    runVonP configPool conf prop

vOnPWith :: (Show d, Resultable d, SAT (ReadableProp d))
  => ReadableSMTConf d -> ReadableProp d -> IO (Result d)
vOnPWith = vOnPWithConf Nothing

pOnVWithConf :: (Resultable d, Show d) => Maybe (DimProp d) -> ReadableSMTConf d -> ReadableProp d -> IO (Result d)
pOnVWithConf Nothing          conf prop = fst' <$> runPonV mempty conf prop
pOnVWithConf dimConfig conf prop =
  do
    configPool <- genConfigPool' dimConfig
    -- mapM_ (putStrLn . show) configPool
    -- putStrLn . show $ (length configPool)
    fst' <$> runPonV configPool conf prop

bfWith :: (Show d, Resultable d,SAT (ReadableProp d)) =>
 ReadableSMTConf d -> ReadableProp d -> IO (Result d)
bfWith = bfWithConf Nothing


bf :: (Show d, Resultable d, SAT (ReadableProp d)) =>
  ReadableProp d -> IO (Result d)
bf = bfWith defConf


adWith :: (Show d, Resultable d, SAT (ReadableProp d))
       => ReadableSMTConf d ->
          (d -> Text)       ->
          ReadableProp d    ->
          IO (Result d)
adWith conf f prop = runAD conf prop f


ad :: (Show d, Resultable d, SAT (ReadableProp d)) =>
  (d -> Readable) -> ReadableProp d -> IO (Result d)
ad = adWith defConf

module Result ( Result (..)
              , ResultProp (..)
              , Resultable
              , AssocList
              , insertToResult
              , insertToSat
              , lookupRes
              , lookupRes_
              , getResSat
              , isDMNull
              , configToUniProp
              , getResult
              , consResultProp
              ) where

import           Control.DeepSeq    (NFData)
import           Data.Sequence
import qualified Data.Map           as M
import           Data.Maybe         (maybe)
import           Data.SBV           (SMTResult (..), defaultSMTCfg,
                                     getModelDictionary)
import           Data.SBV.Control   (CheckSatResult (..), Query, checkSat,
                                     getSMTResult)
import           Data.SBV.Internals (cwToBool)
import           Data.String        (IsString, fromString)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           VProp.Core         (configToProp)
import           VProp.Types        (Config, VProp(..), Var)

-- | a type synonym that represents SBVs return type for getAssocs
type AssocList = [(String, Bool)]

-- | A custom type whose only purpose is to define a monoid instance over VProp
-- with logical or as the concat operation and false as unit. We constrain all
-- variable references to be the same just for the Result type
newtype ResultProp d = ResultProp {getProp :: Seq (VProp d d d)}
  deriving (Show, Eq, Generic)

consResultProp :: VProp d d d -> ResultProp d -> ResultProp d
consResultProp x xs = ResultProp $ x <| (getProp xs)

instance NFData d => NFData (ResultProp d)

instance Semigroup (ResultProp d) where
  (<>) x y = ResultProp $ (getProp x) >< (getProp y)

instance Monoid (ResultProp d) where
  mempty  = ResultProp empty
  mappend = (<>)

instance Resultable Var
instance Resultable String
instance Resultable Text

-- | a type class synonym for constraints required to produce a result
class (IsString a, Eq a, Ord a) => Resultable a

-- | a result is a map of propositions where SAT is a boolean formula on
-- dimensions, the rest of the keys are variables of the prop to boolean
-- formulas on dimensions that dictate the values of those variables be they
-- true or false
newtype Result d = Result {getRes :: M.Map d (ResultProp d)}
  deriving (Show,Eq,Generic)

instance NFData d => NFData (Result d)

instance (Eq d, Ord d) => Semigroup (Result d) where
  x <> y = Result $ M.unionWith (<>) (getRes x) (getRes y)

-- | we define a special key "__Sat" to represent when all dimensions are
-- satisfiable. TODO use type families to properly abstract the key out
instance (Resultable d) => Monoid (Result d) where
  mempty  = Result $ M.singleton "__Sat" mempty
  mappend = (<>)


configToUniProp :: Config d -> ResultProp d
configToUniProp = ResultProp . pure . configToProp

insertToResult :: (Eq d, Ord d) => d -> ResultProp d -> Result d -> Result d
insertToResult k v = Result . M.insertWith (<>)  k v . getRes

insertToSat :: Resultable d => ResultProp d -> Result d -> Result d
insertToSat = insertToResult "__Sat"

lookupRes :: (Eq d, Ord d) => d -> Result d -> ResultProp d
lookupRes k res = maybe mempty id $ M.lookup k (getRes res)

lookupRes_ :: (Eq d, Ord d) => d -> Result d -> ResultProp d
lookupRes_ k res = (M.!) (getRes res) k

getResSat :: Resultable d => Result d -> ResultProp d
getResSat = lookupRes_ "__Sat"

isDMNull :: Resultable d => Result d -> Bool
isDMNull = M.null . getRes

getVSMTModel :: Query (Maybe SMTResult)
getVSMTModel = do cs <- checkSat
                  case cs of
                    Unk   -> error "Unknown Error from solver!"
  -- if unsat the return unsat, just passing default config to get the unsat
  -- constructor TODO return correct conf
                    Unsat -> return .
                                Just $ Unsatisfiable defaultSMTCfg Nothing
                    Sat   -> getSMTResult >>= return . pure

getResult :: Resultable d => (Bool -> ResultProp d) -> Query (Result d)
getResult f =
  do as <- (maybe mempty id . fmap getModelDictionary) <$> getVSMTModel
     return $
       Result (M.foldMapWithKey
               (\k a -> M.singleton (fromString k) (f $ cwToBool a)) as)

module Result ( Result (..)
              , UniformProp (..)
              , Resultable
              , AssocList
              , addToResult
              , lookupRes
              , lookupRes_
              , getResSat
              , isDMNull
              , modifySat
              , assocToResult
              , configToUniProp
              , IsString
              , Hashable
              , getResult
              ) where

import           Control.Arrow      (second, (***))
import           Data.Hashable      (Hashable)
import qualified Data.HashMap.Lazy  as HM
import           Data.Map           (toList)
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
import           VProp.Types        (Config, VProp, Var, false, (|||))

-- | a type synonym that represents SBVs return type for getAssocs
type AssocList = [(String, Bool)]

-- | A custom type whose only purpose is to define a monoid instance over VProp
-- with logical or as the concat operation and false as unit. We constrain all
-- variable references to be the same just for the Result type
newtype UniformProp d = UniformProp {getProp :: VProp d d d}
  deriving (Show, Eq, Generic)

instance Semigroup (UniformProp d) where
  (<>) x y = UniformProp $ (getProp x) ||| (getProp y)

instance Monoid (UniformProp d) where
  mempty  = UniformProp false
  mappend = (<>)

instance Hashable Var
instance Resultable Var
instance Resultable String
instance Resultable Text

-- | a type class synonym for constraints required to produce a result
class (IsString a, Eq a, Hashable a) => Resultable a

-- | a result is a map of propositions where SAT is a boolean formula on
-- dimensions, the rest of the keys are variables of the prop to boolean
-- formulas on dimensions that dictate the values of those variables be they
-- true or false
newtype Result d = Result {getRes :: HM.HashMap d (UniformProp d)}
  deriving (Show,Eq,Generic)

instance (Eq d, Hashable d) => Semigroup (Result d) where
  x <> y = Result $ HM.unionWith (<>) (getRes x) (getRes y)

-- | we define a special key "__Sat" to represent when all dimensions are
-- satisfiable. TODO use type families to properly abstract the key out
instance (Resultable d) => Monoid (Result d) where
  mempty  = Result $ HM.singleton "__Sat" mempty
  mappend = (<>)


configToUniProp :: Config d -> UniformProp d
configToUniProp = UniformProp . configToProp

addToResult :: (Eq d, Hashable d) => d -> UniformProp d -> Result d -> Result d
addToResult k v = Result . HM.insertWith (<>) k v . getRes

lookupRes :: (Eq d, Hashable d) => d -> Result d -> Maybe (UniformProp d)
lookupRes k res = HM.lookup k (getRes res)

lookupRes_ :: (Eq d, Hashable d) => d -> Result d -> UniformProp d
lookupRes_ k res = (HM.!) (getRes res) k

getResSat :: Resultable d => Result d -> UniformProp d
getResSat = lookupRes_ "__Sat"

isDMNull :: Resultable d => Result d -> Bool
isDMNull = maybe False ((==) mempty) . lookupRes "__Sat"

modifySat :: Resultable d => UniformProp d -> Result d -> Result d
modifySat = addToResult "__Sat"

assocToResult :: Resultable d => (Bool -> UniformProp d) -> AssocList -> Result d
assocToResult f xs = Result . HM.fromList $ fmap (fromString *** f) xs

getVSMTModel :: Query (Maybe SMTResult)
getVSMTModel = do cs <- checkSat
                  case cs of
                    Unk   -> error "Unknown Error from solver!"
  -- if unsat the return unsat, just passing default config to get the unsat
  -- constructor TODO return correct conf
                    Unsat -> return .
                                Just $ Unsatisfiable defaultSMTCfg Nothing
                    Sat   -> getSMTResult >>= return . pure

getResult :: Resultable d => (Bool -> UniformProp d) -> Query (Result d)
getResult f = do as <- (maybe mempty id . fmap getModelDictionary) <$> getVSMTModel
                 return $ assocToResult f (second cwToBool <$> toList as)

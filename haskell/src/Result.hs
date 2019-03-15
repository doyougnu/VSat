module Result ( Result(..)
              , ResultProp(..)
              , Resultable(..)
              , UniformProp(..)
              , insertToResult
              , insertToSat
              , lookupRes
              , lookupRes_
              , getResSat
              , isResultNull
              , configToResultProp
              , getResult
              , (&:>)
              , (<:|)
              , (<:&)
              , (|:>)
              , toResultProp
              , consWithOr
              , negateResultProp
              ) where

import           Control.DeepSeq         (NFData, force)
import           Data.Map.Internal.Debug (showTree)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (maybe)
import           Data.SBV                (SMTResult (..), getModelDictionary)
import           Data.SBV.Control        (Query, getSMTResult)
import           Data.SBV.Internals      (cvToBool)
import           Data.String             (IsString, fromString)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

import           VProp.Core              (dimToVar)
import           VProp.Types             (BB_B (..), B_B (..), Config,
                                          VProp (..), Var, Dim)

-- | A custom type whose only purpose is to define a monoid instance over VProp
-- with logical or as the concat operation and false as unit. We constrain all
-- variable references to be the same just for the Result type
newtype UniformProp d = UniformProp {uniProp :: VProp d d d} deriving (Eq,Generic)

instance Show d => Show (UniformProp d) where
  show = show . uniProp

-- | a wrapper adding Nothing to UniformProp. This is essentially building a
-- monoid where mempty in Nothing, and mappend is logical Or. Think of this as a
-- list
newtype ResultProp d = ResultProp {getProp :: Maybe (UniformProp d)}
  deriving (Eq,Generic,Semigroup,Monoid)

instance Show d => Show (ResultProp d) where
  show rp = maybe mempty show $ getProp rp


-- | construct a result prop from a uniformprop, this is just used for a nice
-- api interface
toResultProp :: VProp d d d -> ResultProp d
toResultProp = ResultProp . Just . UniformProp

negateResultProp :: ResultProp d -> ResultProp d
negateResultProp = ResultProp . fmap (UniformProp . OpB Not . uniProp) . getProp

-- | internal only, allows for more flexibility in cons'ing onto a resultProp
consWith :: (VProp d d d -> VProp d d d -> VProp d d d) -> VProp d d d -> ResultProp d -> ResultProp d
consWith f x xs = ResultProp $ do xs' <- getProp xs
                                  let res = f x (uniProp xs')
                                  return $ UniformProp res

-- | cons the first resultProp onto the second with an Or, if the first is
-- larger than the second then this will be O(i). O(1) in the case where the
-- first is a singleton
consWithOr :: VProp d d d -> ResultProp d -> ResultProp d
consWithOr = consWith (OpBB Or)

consWithAnd :: VProp d d d -> ResultProp d -> ResultProp d
consWithAnd = consWith (OpBB And)

-- | O(1) cons result prop infix form with a logical And. Note that this proper
-- use is x :&> y, where |y| << |x| and will result in [y,x]. This is purposeful
-- and for convenience in the Run module. Essentially put the smaller argument where the angle points
infixr 5 &:>
(&:>) :: ResultProp d -> VProp d d d -> ResultProp d
(&:>) = flip consWithAnd

infixr 5 <:&
(<:&) :: VProp d d d -> ResultProp d -> ResultProp d
(<:&) = consWithAnd

-- | O(1) cons result prop infix form with a logical Or
infixr 5 <:|
(<:|) :: VProp d d d -> ResultProp d -> ResultProp d
(<:|) = consWithOr

infixr 5 |:>
(|:>) :: ResultProp d -> VProp d d d -> ResultProp d
(|:>) = flip consWithOr

instance NFData d => NFData (UniformProp d)
instance NFData d => NFData (ResultProp d)

-- | define semigroup for uniform props with an Or. The order here is important
-- so that we avoid essentially cons'ing onto the end of a list. This operation
-- and mappend will prioritize the first argument, x, over y, so if |x| > |y|
-- you'll have an O(n) cons
instance Semigroup (UniformProp d) where
  (<>) x y = UniformProp $ OpBB And (uniProp x) (uniProp y)

instance Resultable Var
instance Resultable String
instance Resultable Text

-- | almost the same as configtoProp in VProp.Core but this is hand written for
-- better asymptotic performance
configToResultProp :: Config a -> ResultProp a
configToResultProp = M.foldMapWithKey step
  where step :: Dim a -> Bool -> ResultProp a
        step d b
          | b = toResultProp prop
          | otherwise = negateResultProp . toResultProp $ prop
          where prop = dimToVar d


-- | a type class synonym for constraints required to produce a result
class (IsString a, Eq a, Ord a) => Resultable a

-- | a result is a map of propositions where SAT is a boolean formula on
-- dimensions, the rest of the keys are variables of the prop to boolean
-- formulas on dimensions that dictate the values of those variables be they
-- true or false
newtype Result d = Result {getRes :: M.Map d (ResultProp d)}
  deriving (Eq,Generic,Monoid)

instance Show d => Show (Result d) where
  show = showTree . getRes

instance NFData d => NFData (Result d)

instance (Eq d, Ord d) => Semigroup (Result d) where
  x <> y = Result $ M.unionWith (<>) (getRes x) (getRes y)

insertWith :: (Eq d, Ord d) => (ResultProp d -> ResultProp d -> ResultProp d) ->
  d -> ResultProp d -> Result d -> Result d
insertWith f k v = Result . M.insertWith f k v . getRes

-- | O(log n + O(1)) insert a resultProp into a result. Uses the Monoid instance of ResultProp i.e. x <> y = x && y
insertToResult :: (Eq d, Ord d) => d -> ResultProp d -> Result d -> Result d
insertToResult = insertWith (<>)

-- | O(1) insert a result prop into the result entry for special Sat variable
insertToSat :: Resultable d => ResultProp d -> Result d -> Result d
insertToSat = insertWith helper "__Sat"
  where
    helper :: ResultProp d -> ResultProp d -> ResultProp d
    helper (getProp -> Nothing)    y = y
    helper (getProp -> Just uprop) y = consWithOr (uniProp uprop) y
    helper _                       _ = ResultProp Nothing


-- | O(log n) given a key lookup the result prop
lookupRes :: (Eq d, Ord d) => d -> Result d -> ResultProp d
lookupRes k res = maybe mempty id $ M.lookup k (getRes res)

-- | unsafe O(log n) lookup Res
lookupRes_ :: (Eq d, Ord d) => d -> Result d -> ResultProp d
lookupRes_ k res = (M.!) (getRes res) k

getResSat :: Resultable d => Result d -> ResultProp d
getResSat = lookupRes_ "__Sat"

-- | O(1) is result empty
isResultNull :: Resultable d => Result d -> Bool
isResultNull = M.null . getRes

-- | grab a vsmt model from SBV, check if it is sat or not, if so return it
getVSMTModel :: Query (Maybe SMTResult)
getVSMTModel = pure <$> getSMTResult

-- | getResult from the query monad, takes a function f that is used to dispatch
-- result bools to resultProps i.e. if the model says variable "x" == True then
-- when f is applied "x" == True result from f. This is used to turn
-- dictionaries into <var> == <formula of dimensions where var is True>
-- associations
getResult :: Resultable d => (Bool -> ResultProp d) -> Query (Result d)
getResult !f =
  do as <- fmap (maybe mempty getModelDictionary) $! getVSMTModel
     return $
       Result (M.foldMapWithKey
               (\k a -> M.singleton (fromString k) (f $ cvToBool a)) as)

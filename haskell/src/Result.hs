module Result ( ResultProp(..)
              , Resultable
              , UniformProp(..)
              , UnSatResult(..)
              , Result(..)
              , ResultMap(..)
              , insertToResult
              , insertToSat
              , lookupRes
              , lookupRes_
              , getResSat
              , isResultNull
              , configToResultProp
              , getResult
              , getResultWith
              , (&:>)
              , (<:|)
              , (<:&)
              , (|:>)
              , toResultProp
              , consWithOr
              , negateResultProp
              ) where

import           Control.DeepSeq         (NFData)
import           Data.Map.Internal.Debug (showTree)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (maybe,fromMaybe)
import           Data.SBV                (SMTResult(..), getModelDictionary)
import           Data.SBV.Control        (Query, getSMTResult)
import           Data.SBV.Internals      (cvToBool)
import           Data.String             (IsString, fromString)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

import           SAT
import           VProp.Core              (dimToVar)
import           VProp.Types             (BB_B (..), B_B (..), Config,
                                          VProp (..), Var, Dim(..))

-- | A custom type whose only purpose is to define a monoid instance over VProp
-- with logical or as the concat operation and false as unit. We constrain all
-- variable references to be the same just for the ResultMap type
newtype UniformProp d = UniformProp {uniProp :: VProp d d d}
  deriving (Eq,Ord,Generic,Boolean)

instance Show d => Show (UniformProp d) where
  show = show . uniProp

-- | a wrapper adding Nothing to UniformProp. This is essentially building a
-- monoid where mempty in Nothing, and mappend is logical Or. Think of this as a
-- list
newtype ResultProp d = ResultProp {getProp :: Maybe (UniformProp d)}
  deriving (Eq,Ord,Generic,Semigroup,Monoid)

instance Show d => Show (ResultProp d) where
  show rp = maybe mempty show $ getProp rp


-- | construct a result prop from a uniformprop, this is just used for a nice
-- api interface
toResultProp :: VProp d d d -> ResultProp d
toResultProp = ResultProp . Just . UniformProp

negateResultProp :: ResultProp d -> ResultProp d
negateResultProp = ResultProp . fmap (UniformProp . OpB Not . uniProp) . getProp

-- | internal only, allows for more flexibility in cons'ing onto a resultProp
consWith :: (UniformProp d -> UniformProp d -> UniformProp d) -> UniformProp d -> ResultProp d -> ResultProp d
consWith f x xs = ResultProp $ do xs' <- getProp xs
                                  let res = f x xs'
                                  return res

-- | cons the first resultProp onto the second with an Or, if the first is
-- larger than the second then this will be O(i). O(1) in the case where the
-- first is a singleton
consWithOr :: UniformProp d -> ResultProp d -> ResultProp d
consWithOr = consWith (|||)

consWithAnd :: UniformProp d -> ResultProp d -> ResultProp d
consWithAnd = consWith (&&&)

-- | O(1) cons result prop infix form with a logical And. Note that this proper
-- use is x :&> y, where |y| << |x| and will result in [y,x]. This is purposeful
-- and for convenience in the Run module. Essentially put the smaller argument where the angle points
infixr 5 &:>
(&:>) :: ResultProp d -> VProp d d d -> ResultProp d
(&:>) x y = consWithAnd (UniformProp y) x

infixr 5 <:&
(<:&) :: VProp d d d -> ResultProp d -> ResultProp d
(<:&) = consWithAnd . UniformProp

-- | O(1) cons result prop infix form with a logical Or
infixr 5 <:|
(<:|) :: VProp d d d -> ResultProp d -> ResultProp d
(<:|) = consWithOr . UniformProp

infixr 5 |:>
(|:>) :: ResultProp d -> VProp d d d -> ResultProp d
(|:>) = flip (<:|)

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
class (IsString a, Eq a, Ord a, Monoid a) => Resultable a

-- | a result is a map of propositions where SAT is a boolean formula on
-- dimensions, the rest of the keys are variables of the prop to boolean
-- formulas on dimensions that dictate the values of those variables be they
-- true or false
newtype ResultMap d = ResultMap {getRes :: M.Map d (ResultProp d)}
  deriving (Eq,Generic,Monoid)

instance Show d => Show (ResultMap d) where
  show = showTree . getRes

instance NFData d => NFData (ResultMap d)
instance NFData d => NFData (UnSatResult d)
instance NFData d => NFData (Result d)

-- | An unsat core is a list of strings as dictated by SBV, see issue #21
type UnSatCore = [String]

-- | a mapping keeping the config and the unsatisfiable core of the config; used
-- in the case of an usat result
newtype UnSatResult d = UnSatResult (M.Map (ResultProp d) UnSatCore)
                      deriving (Eq,Show,Generic,Semigroup,Monoid)

newtype Result d = Result (ResultMap d, UnSatResult d)
                 deriving (Eq,Show,Generic,Semigroup,Monoid)

onResMap :: (ResultMap d -> ResultMap d) -> Result d -> Result d
onResMap f (Result (a,b)) = Result (f a, b)

onUnSatRes :: (UnSatResult d -> UnSatResult d) -> Result d -> Result d
onUnSatRes f (Result (a,b)) = Result (a, f b)

mapToResult :: Resultable d => ResultMap d -> Result d
mapToResult r = onResMap (const r) mempty

unSatToResult :: Resultable d => ResultProp d -> UnSatCore -> Result d
unSatToResult r u = onUnSatRes (const uSatRes) mempty
  where uSatRes = UnSatResult $ M.singleton r u

-- | a bad form global variable for this module, this probably should be a type
-- family. Used as a special variable for the result map to accumulate
-- satisfiable configurations on variational formulas
satKey :: Resultable d => d
satKey = fromString "__Sat"

instance (Resultable d) => Semigroup (ResultMap d) where
  x <> y = ResultMap $ M.unionWithKey helper (getRes x) (getRes y)
    where helper k m1 m2
            | k == satKey = consWithOr' m1 m2
            | otherwise = (<>) m1 m2
          consWithOr' p rp =  p' `consWithOr` rp
            where p' = fromMaybe (UniformProp $ LitB False) $ getProp p

insertWith :: (Eq d, Ord d) => (ResultProp d -> ResultProp d -> ResultProp d) ->
  d -> ResultProp d -> ResultMap d -> ResultMap d
insertWith f k v = ResultMap . M.insertWith f k v . getRes

-- | O(log n + O(1)) insert a resultProp into a result. Uses the Monoid instance of ResultProp i.e. x <> y = x && y
insertToResult :: Resultable d => d -> ResultProp d -> Result d -> Result d
insertToResult d prop = onResMap (insertWith (<>) d prop)

-- | O(1) insert a result prop into the result entry for special Sat variable
insertToSat :: Resultable d => ResultProp d -> Result d -> Result d
insertToSat = onResMap . insertWith helper satKey
  where
    helper :: ResultProp d -> ResultProp d -> ResultProp d
    helper (getProp -> Nothing)    y = y
    helper (getProp -> Just uprop) y = consWithOr uprop y
    helper _                       _ = ResultProp Nothing


-- | O(log n) given a key lookup the result prop
lookupRes :: (Eq d, Ord d) => d -> ResultMap d -> ResultProp d
lookupRes k res = fromMaybe mempty $ M.lookup k (getRes res)

-- | unsafe O(log n) lookup Res
lookupRes_ :: (Eq d, Ord d) => d -> ResultMap d -> ResultProp d
lookupRes_ k res = (M.!) (getRes res) k

getResSat :: Resultable d => ResultMap d -> ResultProp d
getResSat = lookupRes_ satKey

-- | O(1) is result empty
isResultNull :: Resultable d => Result d -> Bool
isResultNull = (==) nullResult

nullResult :: (Resultable d) => Result d
nullResult = Result mempty


-- | grab a vsmt model from SBV, check if it is sat or not, if so return it
getVSMTModel :: Query SMTResult
getVSMTModel = getSMTResult

-- | getResult from the query monad, takes a function f that is used to dispatch
-- result bools to resultProps i.e. if the model says variable "x" == True then
-- when f is applied "x" == True result from f. This is used to turn
-- dictionaries into <var> == <formula of dimensions where var is True>
-- associations
getResultWith :: Resultable d => (Bool -> ResultProp d) -> Query (Result d)
getResultWith !f =
  do model <- getVSMTModel
     return $!
       case model of
         m@(Satisfiable _ _)         ->
           mapToResult . toResMap . getModelDictionary $! m
         (Unsatisfiable _ unsatCore) ->
           -- we apply f to True here because in the case of an unsat we want to
           -- save the proposition that produced the unsat, if we applied to
           -- false then we would have the negation of that proposition for
           -- unsat
           unSatToResult (f True) $! fromMaybe mempty unsatCore
         _                           -> mempty
  where
    toResMap = ResultMap . M.foldMapWithKey
               (\k a -> M.singleton (fromString k) (f $! cvToBool a))


dispatchProp :: ResultProp d -> Bool -> ResultProp d
dispatchProp !p !x = if x
                     then p
                     else negateResultProp p

getResult :: Resultable d => ResultProp d -> Query (Result d)
getResult = getResultWith . dispatchProp

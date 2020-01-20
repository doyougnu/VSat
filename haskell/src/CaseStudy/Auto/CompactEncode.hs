module CaseStudy.Auto.CompactEncode where

import qualified Control.Monad.State.Strict as S
import           Control.Monad              (when, liftM2,liftM)
import           Data.Bitraversable         (bimapM)
import           Data.Maybe                 (isJust,fromJust)
import qualified Data.Map                   as M
import           Data.String                (IsString)
import           Data.List                  as L
import           Data.Text

import           CaseStudy.Auto.Lang
import           CaseStudy.Auto.Auto
import           CaseStudy.Auto.Run
import           VProp.Core                 ()
import qualified VProp.Types                as V


-- mapMmap :: (Ord a2, Monad m) =>
--    (a1 -> m a2) -> (b1 -> m b2) -> M.Map a1 b1 -> m (M.Map a2 b2)
-- mapMmap kf vf = liftM M.fromList . mapM fs . M.assocs
--     where
--     fs (k, v) = liftM2 (,) (kf k) (vf v)

-- -- | given a list of props, run encode them to VProps with holes, then collapse
-- -- the holes by reasoning about the evo_ctx's operators
-- compactEncode :: [AutoLang Text Text] -> V.VProp Text Text Text
-- compactEncode as = conjoin' $ M.foldrWithKey step [] compressedMap
--   where
--     asSorted = makeAssocList as
--     (as', (dimMap,_)) = S.runState (mapM (mapM (bimapM autoToVSat_ autoToVSat_)) asSorted) (M.empty, 0)
--     assocMap = M.unionsWith (V.OpBB V.And) $ (M.fromListWith (flip $ V.OpBB V.And) <$> as')
--     conjoin' = L.foldl' (V.OpBB V.And) V.true
--     compressedMap = compactEncode' dimMap assocMap

--     -- fold the result map by filling in the holes if we have a choice
--     step (V.ChcB d _ r) v acc = (V.ChcB d v r):acc
--     -- if not then its __plain__ and just accumulate the value
--     step _ v acc = v:acc

-- compactEncode' :: (Show d, Show a, Show b, Ord a, Ord b, IsString a, Ord d) =>
--   M.Map (EvoContext a) (V.Dim d) -> M.Map (V.VProp d a b) (V.VProp d a b) -> PackSt d a b
-- compactEncode' dimMap as = S.execState (mapMmap (compact bindings) return as') as'
--   where
--     -- reverse the dimMap to get dims at key position
--     rDimMap = M.foldrWithKey (\k v m -> M.union m (M.singleton v k)) M.empty dimMap
--     -- helper function that nests conjoined choices
--     helper a@(V.OpBB V.And x@(V.ChcB d l r) y@(V.ChcB _ l' _)) v
--       | isHole l && isHole l' = M.singleton x v <> M.singleton y v
--       | otherwise = M.singleton a v
--     helper x v = M.singleton x v
--     bindings = (rDimMap, dimMap)
--     as' = M.foldMapWithKey helper as

-- type PackSt d a b = M.Map (V.VProp d a b) (V.VProp d a b)

-- type DimToEvo d a = M.Map (V.Dim d) (EvoContext a)
-- type EvoToDim d a = M.Map (EvoContext a) (V.Dim d)

-- type Bindings d a = (DimToEvo d a, EvoToDim d a)

-- compact :: (Ord a, Ord d, Ord b, IsString a, Show d, Show a, Show b) =>
--   Bindings d a
--   -> V.VProp d a b
--   -> S.State (PackSt d a b) ()
-- compact bs@(rDimMap, _) (V.ChcB d _ _) =
--   do
--     when (M.member d rDimMap) $
--       do
--         let d' = getLessThan d bs
--         when (isJust d') $
--           do
--             let d'Chc = (V.ChcB (fromJust d') hole V.true)
--             st <- S.get
--             toAdd <- S.gets (M.lookup d'Chc)
--             addToFalse d toAdd
--             remove d'Chc
-- compact _ _ = return ()

-- getLessThan :: (Ord d, Ord a, Show d, Show a) =>
--                V.Dim d
--             -> Bindings d a
--             -> Maybe (V.Dim d)
-- getLessThan d (dte, etd) = res
--   where a = dte M.! d
--         res = go a etd

--         go (LSTE, i) stMap = Just $ stMap M.! (LST, i)
--         go (GRTE, i) stMap = Just $ stMap M.! (GRT, i)
--         go _ _ = Nothing


-- addToFalse :: (Eq d, Ord a, Ord b, Ord d) =>
--   V.Dim d -> Maybe (V.VProp d a b) -> S.State (PackSt d a b) ()
-- addToFalse _ Nothing = return ()
-- addToFalse d (Just toAdd) = S.modify' $ M.mapKeys helper
--   where helper a@(V.ChcB d' (V.ChcB d'' l' r') _)
--           | d'' == d = (V.ChcB d' (V.ChcB d'' l' toAdd) r')
--           | otherwise = a
--         helper a@(V.ChcB d' l' _)
--           | d' == d = (V.ChcB d' l' toAdd)
--           | otherwise = a
--         helper x = x


-- remove :: (Ord a, Ord d, Ord b) =>
--   (V.VProp d a b) -> S.State (PackSt d a b) ()
-- remove = S.modify . M.delete

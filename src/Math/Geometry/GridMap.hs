------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.GridMap
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Ordered maps from tiles on a grid to values.
-- This module is a wrapper around @'Math.Geometry.Grid'@ and 
-- @'Data.Map'@, in order to combine the functionality of grids and maps
-- into a single type.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}

module Math.Geometry.GridMap
  (
    -- * Map classes and types
    GridMap(..),

    -- * Folds
    M.foldr,
    M.foldr',
    M.foldl,
    M.foldl',

    -- * Differences between @GridMap@ and @Map@.
    -- $Compare
  ) where

import Prelude hiding (lookup, map, foldr, foldl, foldr1, foldl1, null)
import Data.Foldable (Foldable)
import qualified Data.Map as M
--import qualified Data.Map.Strict as Strict (Map)
import qualified Math.Geometry.Grid as G

-- | A regular arrangement of tiles, having a value associated with
--   each tile.
--   Minimal complete definition: @toMap@, @toGrid@, @adjustWithKey@,
--   @mapWithKey@.
--
--   Note: Some of the methods have an @Ord@ constraint on the grid 
--   index. This is purely to make it easier to write implementations.
--   While tile positions can be ordered (e.g., @(1,2) < (2,1)@), the
--   ordering may not be particularly meaningful. (Comparisons such as 
--   /east of/ or /south of/ may be more sensible.) However, it is
--   convenient to write implementations of this class using
--   @Data.Map@, with the grid indices as keys. Many of the functions
--   in @Data.Map@ impose the @Ord@ constraint on map keys, so we'll
--   live with it. In summary, to use some methods in this class, your
--   grid indices must be orderable.
class (G.Grid (BaseGrid gm v), Foldable gm) => 
    GridMap (gm :: * -> *) v where
  type BaseGrid gm v

  -- | Find the value at a tile position in the grid.
  (!) :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => gm v -> k -> v
  (!) gm k = toMap gm M.! k

  -- | Returns a map of grid indices to values.
  toMap :: k ~ (G.Index (BaseGrid gm v)) => gm v -> M.Map k v

  -- | Returns the grid on which this map is based.
  toGrid :: gm v -> BaseGrid gm v

  -- | Convert the map to a list of key/value pairs.
  toList :: k ~ (G.Index (BaseGrid gm v)) => gm v -> [(k, v)]
  toList = M.toList . toMap

  -- | Lookup the value at a tile position in the grid map.
  lookup :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => k -> gm v -> Maybe v
  lookup k = M.lookup k . toMap

  -- | Adjust a value at a specific tile position. When the tile is not
  --   within the bounds of the grid map, the original grid map is
  --   returned.
  adjust :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => 
    (v -> v) -> k -> gm v -> gm v
  adjust f = adjustWithKey (\_ v -> f v)

  -- | Adjust a value at a specific tile position. When the tile is not
  --   within the bounds of the grid map, the original grid map is
  --   returned.
  adjustWithKey :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => 
    (k -> v -> v) -> k -> gm v -> gm v

  -- | The expression @('findWithDefault' def k map)@ returns the value
  --   at tile position @k@ or returns @def@ when the tile is not within
  --   the bounds of the grid map.
  findWithDefault :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => 
    v -> k -> gm v -> v
  findWithDefault v k = M.findWithDefault v k . toMap

  -- | Returns all values in the map 
  elems :: gm v -> [v]
  elems = M.elems . toMap

  -- | Map a function over all values in the map.
  map 
    :: (GridMap gm v2, 
        G.Index (BaseGrid gm v) ~ G.Index (BaseGrid gm v2)) => 
    (v -> v2) -> gm v -> gm v2
  map f = mapWithKey (\_ v -> f v)

  -- | Map a function over all values in the map.
  mapWithKey 
    :: (k ~ G.Index (BaseGrid gm v), k ~ G.Index (BaseGrid gm v2), 
        GridMap gm v2) => 
      (k -> v -> v2) -> gm v -> gm v2

{- $Compare
Some functions in @Data.Map@ have been replaced in @GridMap@.
These changes are listed in the table below.

@
Map function        | corresponding GridMap function
--------------------+----------------------------------------------
!                   | !
\\\\                  | See note 1
empty               | 'Math.Geometry.GridMap.Lazy.lazyGridMap' g []
findWithDefault     | 'findWithDefault'
insert              | See notes 1, 2
lookup              | 'lookup'
lookupLE            | See notes 1, 3
lookupLT            | See notes 1, 3
lookupGE            | See notes 1, 3
lookupGT            | See notes 1, 3
member              | 'Math.Geometry.Grid.contains'
notMember           | not 'Math.Geometry.Grid.contains'
null                | 'Math.Geometry.Grid.null'
singleton           | 'lazyGridMap' g [v]
size                | 'Math.Geometry.Grid.size', 'Math.Geometry.Grid.tileCount'
insert              | See notes 1, 2
insertWith          | See notes 1, 2
insertWithKey       | See notes 1, 2
insertLookupWithKey | See notes 1, 2
delete              | See notes 1, 2
adjust              | 'adjust'
adjustWithKey       | 'adjustWithKey'
update              | See notes 1, 2
updateWithKey       | See notes 1, 2
updateLookupWithKey | See notes 1, 2
alter               | See notes 1, 2
union               | See notes 1, 2
unionWith           | See notes 1, 2
unionWithKey        | See notes 1, 2
unions              | See notes 1, 2
unionsWith          | See notes 1, 2
difference          | See notes 1, 2
differenceWith      | See notes 1, 2
differenceWithKey   | See notes 1, 2
intersection        | See notes 1, 2
intersectionWith    | See notes 1, 2
intersectionWithKey | See notes 1, 2
mergeWithKey        | See notes 1, 2
M.map               | 'Math.Geometry.GridMap.Lazy.fmap', or see note 1
mapWithKey          | See note 1
traverseWithKey     | See notes 1, 2
mapAccum            | See note 1
mapAccumWithKey     | See note 1
mapAccumRWithKey    | See note 1
mapKeys             | See note 1
mapKeysWith         | See note 1
mapKeysMonotonic    | See note 1
foldr               | See note 1
foldl               | See note 1
foldrWithKey        | See note 1
foldlWithKey        | See note 1
foldr'              | See note 1
foldl'              | See note 1
foldrWithKey'       | See note 1
foldlWithKey'       | See note 1
elems               | 'elems'
keys                | 'indices'
assocs              | See note 1
keysSet             | See note 1
fromSet             | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
toList              | See note 1
fromList            | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
fromListWithKey     | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
fromListWith        | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
toAscList           | See notes 1, 3
toDescList          | See notes 1, 3
fromAscList         | See notes 1, 3
fromAscListWith     | See notes 1, 3
fromAscListWithKey  | See notes 1, 3
fromDistinctAscList | See notes 1, 3
filter              | See notes 1, 2
filterWithKey       | See notes 1, 2
partition           | See notes 1, 2
partitionWithKey    | See notes 1, 2
mapMaybe            | See notes 1, 2
mapMaybeWithKey     | See notes 1, 2
mapEither           | See notes 1, 2
mapEitherWithKey    | See notes 1, 2
split               | See notes 1, 2
splitLookup         | See notes 1, 2
isSubmapOf          | See note 1
isSubmapOfBy        | See note 1
isProperSubmapOf    | See note 1
isProperSubmapOfBy  | See note 1
lookupIndex         | See note 1
findIndex           | See note 1
elemAt              | See note 1
updateAt            | See note 1
deleteAt            | See notes 1, 2
findMin             | See notes 1, 3
findMax             | See notes 1, 3
deleteMin           | See notes 1, 2, 3
deleteMax           | See notes 1, 2, 3
deleteFindMin       | See notes 1, 2, 3
deleteFindMax       | See notes 1, 2, 3
updateMin           | See notes 1, 2, 3
updateMax           | See notes 1, 2, 3
updateMinWithKey    | See notes 1, 2, 3
updateMaxWithKey    | See notes 1, 2, 3
minView             | See notes 1, 3
maxView             | See notes 1, 3
minViewWithKey      | See notes 1, 2, 3
maxViewWithKey      | See notes 1, 2, 3
showTree            | See note 1
showTreeWith        | See note 1
valid               | See note 1
@

Notes:

1. You can extract the map using @'toMap'@ and apply the function from
@Data.Map@ to the result.

2. Not implemented because the resulting map might have different 
dimensions than the original input @GridMap@(s). However, you can
extract the map using @'toMap'@ and apply the function from @Data.Map@
to the result.

3. Not implemented because, although tile positions can be ordered
(e.g., @(1,2) < (2,1)@), the ordering may not be meaningful for grid 
maps. Comparisons such as /east of/ or /south of/ may be more useful.
However, you can extract the map using @'toMap'@ and apply the function
from @Data.Map@ to the result.
-}



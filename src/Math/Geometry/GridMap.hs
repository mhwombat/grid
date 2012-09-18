-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.GridMap
-- Copyright   :  (c) Amy de Buitléir 2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Ordered maps from tiles on a grid to values.
-- This module is a wrapper around @'Math.Geometry.Grid'@ and @'Data.Map'@,
-- in order to combine the functionality of grids and maps into a single type.
--
-----------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, 
    UndecidableInstances #-}

module Math.Geometry.GridMap
  (
    -- * Differences between @GridMap@ and @Map@.
    -- $Compare

    -- * Map type
    GridMap,

    -- * Construction
    lazyGridMap,

    -- * Grid functions
    indices,
    distance,
    size,
    neighbours,
    inGrid,
    viewpoint,
    tileCount,
    empty,
    nonEmpty,
    

    -- * Map functions
    -- ** Operators
    (!),

    -- ** Query
    lookup,
    findWithDefault,

    -- ** Update
    adjust,
    adjustWithKey,

    -- ** Map
    map,
    mapWithKey,
    mapAccum,
    mapAccumWithKey,

    -- ** Folds
    fold,
    foldWithKey,
    fold',
    foldWithKey',

    -- ** Conversion
    elems,
    keysSet,

    -- ** Lists
    toList
  ) where

import Prelude hiding (lookup, map)
import qualified Data.Map as M
--import qualified Data.Map.Strict as Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Math.Geometry.Grid (Grid(..))

-- | A Map from tile positions in a grid to values. 
data GridMap g k v = LGridMap { toGrid ∷ g, toMap ∷ M.Map k v }
  deriving Eq
-- Future: add alternative constructor for strict maps

instance (Show g, Show v) ⇒ Show (GridMap g k v) where
  show (LGridMap g m) = "lazyGridMap (" ++ show g ++ ") " ++ show (M.elems m)

-- | Construct a grid map which is strict in the keys (tile positions), but
--   lazy in the values.
lazyGridMap ∷ (Ord k, Grid g s k) ⇒ g → [v] → GridMap g k v
lazyGridMap g vs = LGridMap g (M.fromList kvs)
  where kvs = zip ks vs
        ks = indices g

instance (Eq k, Grid g s k) ⇒ Grid (GridMap g k v) s k where
  indices = indices . toGrid
  distance x y = distance x y . toGrid
  size = size . toGrid
  neighbours k = (k `neighbours`) . toGrid
  inGrid k = (k `inGrid`) . toGrid
  viewpoint k = (k `viewpoint`) . toGrid
  tileCount  = tileCount . toGrid
  empty = empty . toGrid
  nonEmpty = nonEmpty . toGrid

-- | /O(min(n,W))/. Find the value at a tile position in the grid.
-- Calls 'error' when the element can not be found.
(!) ∷ Ord k ⇒ GridMap g k v → k → v
(!) m k = toMap m M.! k

modifyMap ∷ (M.Map k a → M.Map k b) → GridMap g k a → GridMap g k b
modifyMap f m = m { toMap = f (toMap m)}

applyToMap ∷ (M.Map k v → c) → GridMap g k v → c
applyToMap f = f . toMap

-- | /O(min(n,W))/. Lookup the value at a tile position in the grid map.
lookup ∷ Ord k ⇒ k → GridMap g k v → Maybe v
lookup k = applyToMap $ M.lookup k

-- | /O(min(n,W))/. Adjust a value at a specific tile position. When the tile
-- is not within the bounds of the grid map, the original grid map is
-- returned.
adjust ∷ Ord k ⇒ (v → v) → k → GridMap g k v → GridMap g k v
adjust f k = modifyMap (M.adjust f k)

-- | /O(min(n,W))/. Adjust a value at a specific key. When the tile
-- is not within the bounds of the grid map, the original grid map is
-- returned.
adjustWithKey ∷ Ord k ⇒ (k → v → v) → k → GridMap g k v → GridMap g k v
adjustWithKey f k = modifyMap (M.adjustWithKey f k)

-- | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
-- returns the value at tile position @k@ or returns @def@ when the tile
-- is not within the bounds of the grid map.
findWithDefault ∷ Ord k ⇒ v → k → GridMap g k v → v
findWithDefault v k m = fromMaybe v $ applyToMap (M.lookup k) m

-- | /O(n)/. Map a function over all values in the grid map.
map ∷ (a → b) → GridMap g k a → GridMap g k b
map f = modifyMap (M.map f)

-- | /O(n)/. Map a function over all values in the grid map.
mapWithKey ∷ (k → a → b) → GridMap g k a → GridMap g k b
mapWithKey f = modifyMap (M.mapWithKey f)

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the grid map.
-- WARNING: The order in which the elements are processed is not guaranteed.
mapAccum ∷ (a → b → (a, c)) → a → GridMap g k b → (a, GridMap g k c)
mapAccum f = mapAccumWithKey (\a _ x → f a x)

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the grid map.
-- WARNING: The order in which the elements are processed is not guaranteed.
mapAccumWithKey ∷ 
  (a → k → b → (a, c)) → a → GridMap g k b → (a, GridMap g k c)
mapAccumWithKey f a gm = (b, gm {toMap=m'})
  where (b, m') = applyToMap (M.mapAccumWithKey f a) gm

-- | /O(n)/. Fold the values in the grid map using the given left-associative
-- binary operator.
-- WARNING: The order in which the elements are processed is not guaranteed.
fold ∷ (a → b → a) → a → GridMap g k b → a
fold f x = applyToMap (M.foldl f x)

-- | /O(n)/. Fold the keys and values in the grid map using the given 
-- left-associative binary operator.
-- WARNING: The order in which the elements are processed is not guaranteed.
foldWithKey ∷ (a → k → b → a) → a → GridMap g k b → a
foldWithKey f x = applyToMap (M.foldlWithKey f x)

-- | /O(n)/. A strict version of 'fold'.
fold' ∷ (a → b → a) → a → GridMap g k b → a
fold' f x = applyToMap (M.foldl' f x)

-- | /O(n)/. A strict version of 'foldWithKey'.
foldWithKey' ∷ (a → k → b → a) → a → GridMap g k b → a
foldWithKey' f x = applyToMap (M.foldlWithKey' f x)

-- | /O(n)/.
-- Return all elements of the grid map. The order is not guaranteed.
elems ∷ GridMap g k a → [a]
elems = applyToMap M.elems

-- | /O(n*min(n,W))/. The set of all tile positions in the grid map.
keysSet ∷ GridMap g k a → Set k
keysSet = applyToMap M.keysSet

-- | /O(n)/. Returns all key (tile position)\/value pairs in the grid map.
toList ∷ GridMap g k a → [(k, a)]
toList = applyToMap M.toList

{- $Compare
Some functions in @Data.Map@ have been replaced in @GridMap@.
These changes are listed in the table below.

@
Map function    | corresponding GridMap function
----------------+-------------------------------
assocs          | 'toList'
empty           | 'lazyGridMap' g []
foldl           | 'fold'
foldl'          | 'fold''
foldlWithKey    | 'foldWithKey'
foldlWithKey'   | 'foldWithKey''
foldr           | 'fold'
foldr'          | 'fold''
foldrWithKey    | 'foldWithKey'
foldrWithKey'   | 'foldWithKey''
fromList        | 'lazyGridMap'
fromListWithKey | 'lazyGridMap'
fromListWith    | 'lazyGridMap'
fromSet         | 'lazyGridMap'
keys            | 'indices'
member          | 'inGrid'
notMember       | not 'inGrid'
null            | 'empty'
singleton       | 'lazyGridMap' g [v]
size            | 'size', 'tileCount'
@

The functions (\\), @alter@, @delete@, @deleteFindMax@, @deleteFindMin@,
@deleteMax@, @deleteMin@, @difference@, @differenceWith@, @differenceWithKey@,
@filter@, @filterWithKey@, @insert@, @insertLookupWithKey@, @insertWith@,
@insertWithKey@, @intersection@, @intersectionWith@, @intersectionWithKey@,
@isProperSubmapOf@, @isProperSubmapOfBy@, @isSubmapOf@, @isSubmapOf@,
@isSubmapOfBy@, @mapEither@, @mapEitherWithKey@, @mapKeys@, @mapKeysWith@,
@mapMaybe@, @mapMaybeWithKey@, @mergeWithKey@, @partition@,
@partitionWithKey@, @split@, @splitLookup@, @traverseWithKey@, @union@,
@unions@, @unionsWith@, @unionWith@, @unionWithKey@, @update@,
@updateLookupWithKey@ and @updateWithKey@ are not implemented because the
resulting map might have different dimensions than the original, or because
they combine maps of different dimensions. 
As a result, these functions may not be as useful for grid maps.
If you need one of these functions, you can extract the map using @toMap@
and apply the function from @Data.Map@ to the result.

The functions @deleteAt@, @elemAt@, @findIndex@, @findMax@, @findMin@, 
@fromAscList@, @fromAscListWith@, @fromAscListWithKey@, @fromDistinctAscList@,
@lookupGE@, @lookupGT@, @lookupIndex@, @lookupLE@, @lookupLT@, 
@mapAccumRWithKey@, @mapKeysMonotonic@, @maxView@, @maxViewWithKey@, 
@minView@, @minViewWithKey@, @toAscList@, @toDescList@, @updateAt@, 
@updateMax@, @updateMaxWithKey@, @updateMin@ and @updateMinWithKey@ are not
implemented because they rely on a meaningful ordering of keys.
While tile positions can be ordered (e.g., @(1,2) < (2,1)@), the ordering
may not be relevant to grid maps.
(Comparisons such as /east of/ or /south of/ may be more meaningful.)
If you need one of these functions, you can extract the map using @toMap@
and apply the function from @Data.Map@ to the result.

The debugging functions @showTree@, @showTreeWith@ and @valid@ are not
implemented.
If you need one of these functions, you can extract the map using @toMap@
and apply the function from @Data.Map@ to the result.
-}



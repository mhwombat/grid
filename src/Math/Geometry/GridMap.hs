------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.GridMap
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Ordered maps from tiles on a grid to values.
-- This module is a wrapper around @'Math.Geometry.Grid'@ and 
-- @'Data.Map'@, in order to combine the functionality of grids and maps
-- into a single type.
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

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
import qualified Data.Map as M
import qualified Math.Geometry.Grid as G

#if MIN_VERSION_base(4,8,0)
#else
import Data.Foldable (Foldable)
#endif

-- | A regular arrangement of tiles, having a value associated with
--   each tile.
--   Minimal complete definition: @toMap@, @toGrid@, @insertWithKey@,
--   @delete@, @adjustWithKey@, @alter@, @mapWithKey@, @filterWithKey@.
--
--   Once a @'GridMap'@ is created, the underlying grid is /fixed/;
--   tiles cannot be added or removed. However, values can be added
--   to empty tiles, and the value at a tile can be modified or
--   removed.
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
  --   Calls error if the tile is not in the grid, or if the tile does
  --   not have an associated value.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["red","blue"]
  -- > λ> m ! (0,0)
  -- > "red"
  -- > λ> m ! (0,5)
  -- > "*** Exception: Map.!: given key is not an element in the map
  (!) :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => gm v -> k -> v
  (!) gm k = toMap gm M.! k

  -- | Returns a map of tile positions to values.
  --
  -- > λ> toMap $ lazyGridMap (rectSquareGrid 1 2) ["red", "blue"]
  -- > fromList [((0,0),"red"),((1,0),"blue")]
  toMap :: k ~ (G.Index (BaseGrid gm v)) => gm v -> M.Map k v

  -- | Returns the grid on which this map is based.
  --
  -- > λ> toGrid $ lazyGridMap (rectSquareGrid 1 2) ["red", "blue"]
  -- > rectSquareGrid 1 2
  toGrid :: gm v -> BaseGrid gm v

  -- | Convert the map to a list of key/value pairs.
  --
  -- > λ> toList $ lazyGridMap (rectSquareGrid 1 2) ["red", "blue"]
  -- > [((0,0),"red"),((1,0),"blue")]
  toList :: k ~ (G.Index (BaseGrid gm v)) => gm v -> [(k, v)]
  toList = M.toList . toMap

  -- | The expression @'lookup' k m@ returns the value contained in the
  --   tile at position @k@ in the map @m@.
  --   If the tile does not contain a value, or is outside the map
  --   bounds, @Nothing@ is returned.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["red","blue"]
  -- > λ> Math.Geometry.GridMap.lookup (1,0) m
  -- > Just "blue"
  -- > λ> Math.Geometry.GridMap.lookup (5,5) m
  -- > Nothing
  lookup :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => k -> gm v -> Maybe v
  lookup k = M.lookup k . toMap

  -- | Insert a new value at a tile position in the grid map.
  --   If the tile already contains a value, the value is replaced.
  --
  -- > λ> insert (1,0) "hello" $ lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > lazyGridMap (rectSquareGrid 1 2) ["red","hello"]
  -- > λ> insert (1,0) "hello" $ lazyGridMap (rectSquareGrid 1 2) ["red","blue"]
  -- > lazyGridMap (rectSquareGrid 1 2) ["red","hello"]
  -- > λ> insert (5,5) "hello" $ lazyGridMap (rectSquareGrid 1 2) ["red","blue"]
  -- > lazyGridMap (rectSquareGrid 1 2) ["red","blue"]
  insert :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => k -> v -> gm v -> gm v
  insert = insertWith const

  -- | The expression @'insertWith' f k v m@ will insert the value
  --   @v@ into the tile at position @k@ if the tile does not already
  --   contain a value.
  --   If the tile does contain a value, it is replaced with
  --   @f v old_value@.
  --   If the tile is not within the bounds of the grid map,
  --   the original grid map is returned.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) [100]
  -- > λ> insertWith (+) (0,0) 1 m
  -- > lazyGridMap (rectSquareGrid 1 2) [101]
  -- > λ> insertWith (+) (1,0) 1 m
  -- > lazyGridMap (rectSquareGrid 1 2) [100,1]
  -- > λ> insertWith (+) (5,5) 1 m
  -- > lazyGridMap (rectSquareGrid 1 2) [100]
  insertWith :: (k ~ (G.Index (BaseGrid gm v)), Ord k) =>
    (v -> v -> v) -> k -> v -> gm v -> gm v
  insertWith f = insertWithKey (\_ x' y' -> f x' y')

  -- | The expression @'insertWithKey' f k v m@ will insert the value
  --   @v@ into the tile at position @k@ if the tile does not already
  --   contain a value.
  --   If the tile does contain a value, it is replaced with
  --   @f k v old_value@.
  --   If the tile is not within the bounds of the grid map,
  --   the original grid map is returned.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> let f k x y = show k ++ " " ++ x ++ y
  -- > λ> insertWithKey f (0,0) "dark" m
  -- > lazyGridMap (rectSquareGrid 1 2) ["(0,0) darkred"]
  -- > λ> insertWithKey f (1,0) "dark" m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red","dark"]
  -- > λ> insertWithKey f (5,5) "dark" m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red"]
  insertWithKey :: (k ~ (G.Index (BaseGrid gm v)), Ord k) =>
    (k -> v -> v -> v) -> k -> v -> gm v -> gm v

  -- | Combines @'lookup'@ with @'insertWithKey'@.
  --   The old value is returned, along with the updated map.
  insertLookupWithKey :: (k ~ (G.Index (BaseGrid gm v)), Ord k) =>
    (k -> v -> v -> v) -> k -> v -> gm v -> (Maybe v, gm v)
  insertLookupWithKey f k v gm = (lookup k gm, insertWithKey f k v gm)

  -- | Deletes the value at a tile position in the grid map.
  --   The tile is not removed from the grid.
  --   If the tile is not within the bounds of the grid map,
  --   the original grid map is returned.
  --   Note: Although this function may remove values, it never removes
  --   tiles from the underlying grid.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> delete (0,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) []
  -- > λ> delete (1,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> delete (5,5) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red"]
  delete :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => k -> gm v -> gm v

  -- | Adjust a value at a specific tile position.
  --   If the tile does not contain a value,
  --   or is not within the bounds of the grid map,
  --   the original grid map is returned.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["world"]
  -- > λ> let f x = "hello " ++ x
  -- > λ> adjust f (0,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["hello world"]
  -- > λ> adjust f (1,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["world"]
  -- > λ> adjust f (5,5) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["world"]
  adjust :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => 
    (v -> v) -> k -> gm v -> gm v
  adjust f = adjustWithKey (\_ v -> f v)

  -- | Adjust a value at a specific tile position. If the tile is not
  --   within the bounds of the grid map, the original grid map is
  --   returned.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["world"]
  -- > λ> let f k x = "Hello, " ++ x ++ " from " ++ show k
  -- > λ> adjustWithKey f (0,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["Hello, world from (0,0)"]
  -- > λ> adjustWithKey f (1,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["world"]
  -- > λ> adjustWithKey f (5,5) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["world"]
  adjustWithKey :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => 
    (k -> v -> v) -> k -> gm v -> gm v

  -- | The expression (alter f k map) alters the value at k, or absence
  --   thereof.
  --   If the tile is not within the bounds of the grid map,
  --   the original grid map is returned.
  --   Can be used to insert, delete, or update a value.
  --   Note: Although this function may remove values, it never removes
  --   tiles from the underlying grid.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> let f _ = Nothing
  -- > λ> alter f (1,0) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> alter f (0,0) m -- deleting a value
  -- > lazyGridMap (rectSquareGrid 1 2) []
  -- > λ> alter f (5,5) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> let f _ = Just "hi!"
  -- > λ> alter f (1,0) m -- inserting a value
  -- > lazyGridMap (rectSquareGrid 1 2) ["red","hi!"]
  -- > λ> alter f (0,0) m -- updating a value
  -- > lazyGridMap (rectSquareGrid 1 2) ["hi!"]
  -- > λ> alter f (5,5) m
  -- > lazyGridMap (rectSquareGrid 1 2) ["red"]
  alter :: (k ~ (G.Index (BaseGrid gm v)), Ord k) =>
    (Maybe v -> Maybe v) -> k -> gm v -> gm v

  -- | The expression @('findWithDefault' def k map)@ returns the value
  --   at tile position @k@ or returns @def@ when the tile is not within
  --   the bounds of the grid map.
  --
  -- > λ> let m = lazyGridMap (rectSquareGrid 1 2) ["red"]
  -- > λ> findWithDefault "yellow" (0,0) m
  -- > "red"
  -- > λ> findWithDefault "yellow" (1,0) m
  -- > "yellow"
  -- > λ> findWithDefault "yellow" (5,5) m
  -- > "yellow"
  findWithDefault :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => 
    v -> k -> gm v -> v
  findWithDefault v k = M.findWithDefault v k . toMap

  -- | Returns the position of all tiles in the map that contain a
  --   value.
  --   To get a list of all tiles in the map regardless of whether or
  --   not they contain values, use @'Math.Geometry.Grid.indices'@.
  keys :: (k ~ (G.Index (BaseGrid gm v)), Ord k) => gm v -> [k]
  keys = M.keys . toMap
  
  -- | Returns all values in the map.
  elems :: gm v -> [v]
  elems = M.elems . toMap

  -- | Maps a function over all values in the map.
  --
  -- > λ> Math.Geometry.GridMap.map (++ "!") $ lazyGridMap (rectSquareGrid 1 3) ["red","blue"]
  -- > lazyGridMap (rectSquareGrid 1 3) ["red!","blue!"]
  map 
    :: (GridMap gm v2, 
        G.Index (BaseGrid gm v) ~ G.Index (BaseGrid gm v2)) => 
    (v -> v2) -> gm v -> gm v2
  map f = mapWithKey (\_ v -> f v)

  -- | Maps a function over all values in the map.
  --
  -- > λ> let f k v = v ++ "@" ++ show k
  -- > λ> mapWithKey f $ lazyGridMap (rectSquareGrid 1 3) ["red","blue"]
  -- > lazyGridMap (rectSquareGrid 1 3) ["red@(0,0)","blue@(1,0)"]
  mapWithKey 
    :: (k ~ G.Index (BaseGrid gm v), k ~ G.Index (BaseGrid gm v2), 
        GridMap gm v2) => 
      (k -> v -> v2) -> gm v -> gm v2

  -- | Return a map containing only the values that satisfy the
  --   predicate.
  --   Note: Although this function may remove values, it never removes
  --   tiles from the underlying grid.
  --
  --
  -- > λ> Math.Geometry.GridMap.filter (> 100) $ lazyGridMap (rectSquareGrid 1 4) [99, 100, 101, 102]
  -- > lazyGridMap (rectSquareGrid 1 4) [101,102]
  filter :: (v -> Bool) -> gm v -> gm v
  filter p = filterWithKey (\_ x -> p x)
    
  -- | Return a map containing only the values that satisfy the
  --   predicate, which may depend on a tile's index as well as its
  --   value.
  --   Note: Although this function may remove values, it never removes
  --   tiles from the underlying grid.
  --
  -- > λ> let f k v = k > (2,0) && v > 100
  -- > λ> filterWithKey f $ lazyGridMap (rectSquareGrid 1 4) [99, 100, 101, 102]
  -- > lazyGridMap (rectSquareGrid 1 4) [102]
  filterWithKey
    :: k ~ (G.Index (BaseGrid gm v)) =>
      (k -> v -> Bool) -> gm v -> gm v

{- $Compare
Some functions in @Data.Map@ are not currently implemented in @GridMap@.
These differences are listed in the table below.

@
Map function        | corresponding GridMap function
--------------------+----------------------------------------------
!                   | !
\\\\                  | See notes 1, 2
adjust              | 'adjust'
adjustWithKey       | 'adjustWithKey'
alter               | 'alter'
assocs              | See note 1
delete              | 'delete'
deleteAt            | See note 3
deleteFindMax       | See note 3
deleteFindMin       | See note 3
deleteMax           | See note 3
deleteMin           | See note 3
difference          | See notes 1, 4
differenceWith      | See notes 1, 4
differenceWithKey   | See notes 1, 4
elemAt              | See notes 1, 3
elems               | 'elems'
empty               | 'empty'
filter              | 'filter'
filterWithKey       | 'filterWithKey'
findIndex           | See notes 1, 3
findMax             | See notes 1, 3
findMin             | See notes 1, 3
findWithDefault     | 'findWithDefault'
foldl               | See note 1
foldl'              | See note 1
foldlWithKey        | See note 1
foldlWithKey'       | See note 1
foldr               | See note 1
foldr'              | See note 1
foldrWithKey        | See note 1
foldrWithKey'       | See note 1
fromAscList         | See notes 1, 3
fromAscListWith     | See notes 1, 3
fromAscListWithKey  | See notes 1, 3
fromDistinctAscList | See notes 1, 3
fromList            | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
fromListWith        | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
fromListWithKey     | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
fromSet             | 'Math.Geometry.GridMap.Lazy.lazyGridMap'
insert              | 'insert'
insertLookupWithKey | 'insertLookupWithKey'
insertWith          | 'insertWith'
insertWithKey       | 'insertWithKey'
intersection        | See notes 1, 2
intersectionWithKey | See notes 1, 2
intersectionWith    | See notes 1, 2
isProperSubmapOf    | See note 1
isProperSubmapOfBy  | See note 1
isSubmapOf          | See note 1
isSubmapOfBy        | See note 1
keys                | 'indices'
keysSet             | See note 1
lookup              | 'lookup'
lookupGE            | See notes 1, 3
lookupGT            | See notes 1, 3
lookupIndex         | See notes 1, 3
lookupLE            | See notes 1, 3
lookupLT            | See notes 1, 3
map                 | 'map'
mapAccum            | See notes 1, 3
mapAccumRWithKey    | See notes 1, 3
mapAccumWithKey     | See notes 1, 3
mapEither           | See note 1
mapEitherWithKey    | See note 1
mapKeys             | See notes 1, 2
mapKeysMonotonic    | See notes 1, 2
mapKeysWith         | See notes 1, 2
mapMaybe            | See note 1
mapMaybeWithKey     | See note 1
mapWithKey          | 'mapWithKey'
maxView             | See notes 1, 3
maxViewWithKey      | See notes 1, 3
member              | 'Math.Geometry.Grid.contains'
mergeWithKey        | See notes 1, 2
minView             | See notes 1, 3
minViewWithKey      | See notes 1, 3
notMember           | not 'Math.Geometry.Grid.contains'
null                | To find out if a grid has no /values/, extract the
                    | map using 'toMap' and apply 'Data.Map.null' to
                    | the result. To find out if a grid has no /tiles/,
                    | use 'Math.Geometry.Grid.null'.
partition           | See notes 1, 2
partitionWithKey    | See notes 1, 2
showTree            | See note 1
showTreeWith        | See note 1
singleton           | 'lazyGridMap' g [v]
size                | To find out the number of /values/ in a grid,
                    | extract the values using 'toList' and apply
                    | 'length' to the result. To find out the
                    | number of /tiles/, 'Math.Geometry.Grid.tileCount'.
                    | To find out the dimensions of the grid, use
                    | 'Math.Geometry.Grid.size'.
split               | See notes 1, 2, 3
splitLookup         | See notes 1, 2, 3
toAscList           | See notes 1, 3
toDescList          | See notes 1, 3
toList              | See note 1
traverseWithKey     | See notes 1, 2
union               | See notes 1, 2
unions              | See notes 1, 2
unionsWith          | See notes 1, 2
unionWithKey        | See notes 1, 2
unionWith           | See notes 1, 2
updateAt            | See notes 1, 3
updateLookupWithKey | See note 1
updateMax           | See notes 1, 3
updateMaxWithKey    | See notes 1, 3
updateMin           | See notes 1, 3
updateMinWithKey    | See notes 1, 3
update              | See note 1
updateWithKey       | See note 1
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

4. It's not obvious what the behaviour should be if the two maps have
different underlying grids. Different users may want different
behaviour.
-}



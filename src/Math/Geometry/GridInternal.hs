------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.GridInternal
-- Copyright   :  (c) Amy de Buitléir 2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @Grid@ internals. Most developers should
-- use @Grid@ instead. This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Math.Geometry.GridInternal where

import Prelude hiding (null)

import Data.Function (on)
import Data.List ((\\), groupBy, nub, nubBy, sortBy)
import Data.Ord (comparing)

-- | A regular arrangement of tiles.
--   Minimal complete definition: @'Index'@, @'Direction'@, @'indices'@,
--   @'distance'@, @'directionTo'@.
class Grid g where
  type Index g
  type Direction g

  -- | Returns the indices of all tiles in a grid.
  indices :: g -> [Index g]

  -- | @'distance' g a b@ returns the minimum number of moves required
  --   to get from the tile at index @a@ to the tile at index @b@ in
  --   grid @g@, moving between adjacent tiles at each step. (Two tiles
  --   are adjacent if they share an edge.) If @a@ or @b@ are not
  --   contained within @g@, the result is undefined.
  distance :: g -> Index g -> Index g -> Int

  -- | @'minDistance' g bs a@ returns the minimum number of moves
  --   required to get from any of the tiles at indices @bs@ to the tile
  --   at index @a@ in grid @g@, moving between adjacent tiles at each
  --   step. (Two tiles are adjacent if they share an edge.) If @a@ or
  --   any of @bs@ are not contained within @g@, the result is
  --   undefined.
  minDistance :: g -> [Index g] -> Index g -> Int
  minDistance = defaultMinDistance

  -- | @'neighbours' g a@ returns the indices of the tiles in the grid
  --   @g@ which are adjacent to the tile with index @a@.
  neighbours :: Eq (Index g) => g -> Index g -> [Index g]
  neighbours = defaultNeighbours

  -- | @'neighboursOfSet' g as@ returns the indices of the tiles in the
  --   grid @g@ which are adjacent to any of the tiles with index in
  --   @as@.
  neighboursOfSet :: Eq (Index g) => g -> [Index g] -> [Index g]
  neighboursOfSet = defaultNeighboursOfSet

  -- | @'neighbour' g d a@ returns the indices of the tile in the grid
  --   @g@ which is adjacent to the tile with index @a@, in the
  --   direction @d@.
  neighbour
    :: (Eq (Index g), Eq (Direction g))
       => g -> Index g -> Direction g -> Maybe (Index g)
  neighbour = defaultNeighbour

  -- | @'numNeighbours' g a@ returns the number of tiles in the grid
  --   @g@ which are adjacent to the tile with index @a@.
  numNeighbours :: Eq (Index g) => g -> Index g -> Int
  numNeighbours g = length . neighbours g

  -- | @g `'contains'` a@ returns @True@ if the index @a@ is contained
  --   within the grid @g@, otherwise it returns false.
  contains :: Eq (Index g) => g -> Index g -> Bool
  contains g a = a `elem` indices g

  -- | Returns the number of tiles in a grid. Compare with @'size'@.
  tileCount :: g -> Int
  tileCount = length . indices

  -- | Returns @True@ if the number of tiles in a grid is zero, @False@
  --   otherwise.
  null :: g -> Bool
  null g = tileCount g == 0

  -- | Returns @False@ if the number of tiles in a grid is zero, @True@
  --   otherwise.
  nonNull :: g -> Bool
  nonNull = not . null

  -- | A list of all edges in a grid, where the edges are represented by
  --   a pair of indices of adjacent tiles.
  edges :: Eq (Index g) => g -> [(Index g,Index g)]
  edges = defaultEdges

  -- | @'viewpoint' g a@ returns a list of pairs associating the index
  --   of each tile in @g@ with its distance to the tile with index @a@.
  --   If @a@ is not contained within @g@, the result is undefined.
  viewpoint :: g -> Index g -> [(Index g, Int)]
  viewpoint g p = map f (indices g)
    where f a = (a, distance g p a)

  -- | @'isAdjacent' g a b@ returns @True@ if the tile at index @a@ is
  --   adjacent to the tile at index @b@ in @g@. (Two tiles are adjacent
  --   if they share an edge.) If @a@ or @b@ are not contained within
  --   @g@, the result is undefined.
  isAdjacent :: g -> Index g -> Index g -> Bool
  isAdjacent = defaultIsAdjacent

  -- | @'adjacentTilesToward' g a b@ returns the indices of all tiles
  --   which are neighbours of the tile at index @a@, and which are
  --   closer to the tile at @b@ than @a@ is. In other words, it returns
  --   the possible next steps on a minimal path from @a@ to @b@. If @a@
  --   or @b@ are not contained within @g@, or if there is no path from
  --   @a@ to @b@ (e.g., a disconnected grid), the result is undefined.
  adjacentTilesToward :: Eq (Index g) => g -> Index g -> Index g -> [Index g]
  adjacentTilesToward = defaultAdjacentTilesToward

  -- | @'minimalPaths' g a b@ returns a list of all minimal paths from
  --   the tile at index @a@ to the tile at index @b@ in grid @g@. A
  --   path is a sequence of tiles where each tile in the sequence is
  --   adjacent to the previous one. (Two tiles are adjacent if they
  --   share an edge.) If @a@ or @b@ are not contained within @g@, the
  --   result is undefined.
  --
  --   Tip: The default implementation of this function calls
  --   @'adjacentTilesToward'@. If you want to use a custom algorithm,
  --   consider modifying @'adjacentTilesToward'@ instead of
  --   @'minimalPaths'@.
  minimalPaths :: Eq (Index g) => g -> Index g -> Index g -> [[Index g]]
  minimalPaths = defaultMinimalPaths

  -- | @'directionTo' g a b@ returns the direction(s) of the next
  --   tile(s) in a /minimal/ path from the tile at index @a@ to the
  --   tile at index @b@ in grid @g@.
  directionTo :: g -> Index g -> Index g -> [Direction g]

  --
  -- These default implementations are broken out to make it easier to
  -- compare the results with custom implementations (for testing).
  --
  
  defaultMinDistance :: g -> [Index g] -> Index g -> Int
  defaultMinDistance g xs a = minimum . map (distance g a) $ xs

  -- WARNING: this implementation won't work for wrapped grids
  defaultNeighbours :: g -> Index g -> [Index g]
  defaultNeighbours g a = filter (\b -> distance g a b == 1 ) $ indices g

  -- This should work for wrapped grids, though.
  defaultNeighboursOfSet :: Eq (Index g) => g -> [Index g] -> [Index g]
  defaultNeighboursOfSet g as = ns \\ as
    where ns = nub . concatMap (neighbours g) $ as

  -- WARNING: this implementation won't work for wrapped grids
  defaultNeighbour :: (Eq (Index g), Eq (Direction g))
    => g -> Index g -> Direction g -> Maybe (Index g)
  defaultNeighbour g a d =
    maybeHead . filter (\b -> [d] == directionTo g a b) . neighbours g $ a
    where maybeHead (x:_) = Just x
          maybeHead _ = Nothing

  defaultTileCount :: g -> Int
  defaultTileCount = length . indices

  -- WARNING: this implementation won't work for wrapped grids
  defaultEdges :: Eq (Index g) => g -> [(Index g,Index g)]
  defaultEdges g = nubBy sameEdge $ concatMap (`adjacentEdges` g) $ indices g

  -- WARNING: this implementation won't work for wrapped grids
  defaultIsAdjacent :: g -> Index g -> Index g -> Bool
  defaultIsAdjacent g a b = distance g a b == 1

  defaultAdjacentTilesToward
    :: Eq (Index g) => g -> Index g -> Index g -> [Index g]
  defaultAdjacentTilesToward g a b = filter f $ neighbours g a
    where f c = distance g c b == distance g a b - 1

  defaultMinimalPaths :: Eq (Index g)
    => g -> Index g -> Index g -> [[Index g]]
  defaultMinimalPaths g a b
    | a == b              = [[a]]
    | distance g a b == 1 = [[a,b]]
    | otherwise          = map (a:) xs
    where xs = concatMap (\c -> minimalPaths g c b) ys
          ys = adjacentTilesToward g a b

-- | A regular arrangement of tiles where the number of tiles is finite.
--   Minimal complete definition: @'size'@, @'maxPossibleDistance'@.
class Grid g => FiniteGrid g where
  type Size s
  -- | Returns the dimensions of the grid.
  --   For example, if @g@ is a 4x3 rectangular grid, @'size' g@ would
  --   return @(4, 3)@, while @'tileCount' g@ would return @12@.
  size :: g -> Size g
  -- | Returns the largest possible distance between two tiles in the
  --   grid.
  maxPossibleDistance :: g -> Int


-- | A regular arrangement of tiles with an edge.
--   Minimal complete definition: @'tileSideCount'@.
class Grid g => BoundedGrid g where
  -- | Returns the number of sides a tile has
  tileSideCount :: g -> Int

  -- | Returns a the indices of all the tiles at the boundary of a grid.
  boundary :: Eq (Index g) => g -> [Index g]
  boundary = defaultBoundary

  -- | @'isBoundary' g a@' returns @True@ if the tile with index @a@ is
  --   on a boundary of @g@, @False@ otherwise. (Corner tiles are also
  --   boundary tiles.)
  isBoundary :: Eq (Index g) => g -> Index g -> Bool
  isBoundary = defaultIsBoundary

  -- | Returns the index of the tile(s) that require the maximum number
  --   of moves to reach the nearest boundary tile. A grid may have more
  --   than one central tile (e.g., a rectangular grid with an even
  --   number of rows and columns will have four central tiles).
  centre :: Eq (Index g) => g -> [Index g]
  centre = defaultCentre

  -- | @'isCentre' g a@' returns @True@ if the tile with index @a@ is
  --   a centre tile of @g@, @False@ otherwise.
  isCentre :: Eq (Index g) => g -> Index g -> Bool
  isCentre = defaultIsCentre

  --
  -- These default implementations are broken out to make it easier to
  -- compare the results with custom implementations (for testing).
  --

  defaultBoundary :: Eq (Index g) => g -> [Index g]
  defaultBoundary g = map fst . filter f $ xds
    where xds = map (\b -> (b, numNeighbours g b)) $ indices g
          f (_,n) = n < tileSideCount g

  defaultIsBoundary :: Eq (Index g) => g -> Index g -> Bool
  defaultIsBoundary g a = a `elem` boundary g

  -- WARNING: this implementation won't work for triangular grids.
  -- It probably only works on grids where all the tiles have the same
  -- shape/orientation.
  defaultCentre :: Eq (Index g) => g -> [Index g]
  defaultCentre g = map fst . head . groupBy ((==) `on` snd) .
                sortBy (comparing snd) $ xds
    where xds = map (\b -> (b, f b)) $ indices g
          bs = boundary g
          f x = sum . map (distance g x) $ bs

  defaultIsCentre :: Eq (Index g) => g -> Index g -> Bool
  defaultIsCentre g a = a `elem` centre g
  
-- | A regular arrangement of tiles where the boundaries are joined.
--   Minimal complete definition: @'normalise'@ and @'denormalise'@.
class (Grid g) => WrappedGrid g where
  -- | @'normalise' g a@ returns the "normal" indices for @a@.
  --   TODO: need a clearer description and an illustration.
  normalise :: g -> Index g -> Index g
  -- | @'denormalise' g a@ returns all of the indices in @a@'s
  --   translation group. In other words, it returns @a@ plus the
  --   indices obtained by translating @a@ in each direction by the
  --   extent of the grid along that direction.
  --   TODO: need a clearer description and an illustration.
  denormalise :: g -> Index g -> [Index g]

neighboursBasedOn
  :: (Eq (Index u), Grid g, Grid u, Index g ~ Index u) =>
    u -> g -> Index g -> [Index g]
neighboursBasedOn u g = filter (g `contains`) . neighbours u

distanceBasedOn
  :: (Eq (Index g), Grid g, Grid u, Index g ~ Index u) =>
    u -> g -> Index g -> Index g -> Int
distanceBasedOn u g a b =
  if g `contains` a && g `contains` b
    then distance u a b
    else undefined

directionToBasedOn
  :: (Eq (Index g), Eq (Direction g), Grid g, Grid u, Index g ~ Index u,
    Direction g ~ Direction u) =>
    u -> g -> Index g -> Index g -> [Direction g]
directionToBasedOn u g a b =
  if g `contains` a && g `contains` b
    then nub . concatMap (directionTo u a) . adjacentTilesToward g a $ b
    else undefined

neighboursWrappedBasedOn
  :: (Eq (Index g), WrappedGrid g, Grid u, Index g ~ Index u) =>
    u -> g -> Index g -> [Index g]
neighboursWrappedBasedOn u g =
  filter (g `contains`) . nub . map (normalise g) . neighbours u

neighbourWrappedBasedOn
  :: (Eq (Index g), Eq (Direction g), WrappedGrid g, Grid u,
    Index g ~ Index u, Direction g ~ Direction u) =>
    u -> g -> Index g -> Direction g -> Maybe (Index g)
neighbourWrappedBasedOn u g a d =
  if g `contains` a
    then neighbour u a d >>= return . normalise g
    else Nothing

distanceWrappedBasedOn
  :: (Eq (Index g), WrappedGrid g, Grid u, Index g ~ Index u) =>
    u -> g -> Index g -> Index g -> Int
distanceWrappedBasedOn u g a b =
  if g `contains` a && g `contains` b
    then minimum . map (distance u a') $ bs
    else undefined
  where a' = normalise g a
        bs = denormalise g b

directionToWrappedBasedOn
  :: (Eq (Index g), Eq (Direction g), WrappedGrid g, Grid u,
    Index g ~ Index u, Direction g ~ Direction u) =>
    u -> g -> Index g -> Index g -> [Direction g]
directionToWrappedBasedOn u g a b =
  if g `contains` a && g `contains` b
    then nub . concatMap (directionTo u a') $ ys'
    else undefined
  where a' = normalise g a
        ys = denormalise g b
        minD = distance g a b
        ys' = filter (\c -> distance u a' c == minD) ys

--
-- Helper functions
--

sameEdge :: Eq t => (t, t) -> (t, t) -> Bool
sameEdge (a,b) (c,d) = (a,b) == (c,d) || (a,b) == (d,c)

adjacentEdges :: (Grid g, Eq (Index g)) => Index g -> g -> [(Index g, Index g)]
adjacentEdges i g = map (\j -> (i,j)) $ neighbours g i

cartesianIndices
  :: (Enum r, Enum c, Num r, Num c, Ord r, Ord c) =>
     (r, c) -> [(c, r)]
cartesianIndices (r, c) = west ++ north ++ east ++ south
  where west = [(0,k) | k <- [0,1..r-1], c>0]
        north = [(k,r-1) | k <- [1,2..c-1], r>0]
        east = [(c-1,k) | k <- [r-2,r-3..0], c>1]
        south = [(k,0) | k <- [c-2,c-3..1], r>1]

cartesianCentre :: (Int, Int) -> [(Int, Int)]
cartesianCentre (r,c) = [(i,j) | i <- cartesianMidpoints c, j <- cartesianMidpoints r]

cartesianMidpoints :: Int -> [Int]
cartesianMidpoints k = if even k then [m-1,m] else [m]
  where m = k `div` 2

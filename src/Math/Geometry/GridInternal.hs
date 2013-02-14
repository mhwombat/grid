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
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, 
    FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, 
    FlexibleContexts #-}

module Math.Geometry.GridInternal
  (
    -- * Generic
    Grid(..),
    BoundedGrid(..),
    -- * Grids with triangular tiles
    TriTriGrid,
    triTriGrid,
    ParaTriGrid,
    paraTriGrid,
    -- * Grids with square tiles
    RectSquareGrid,
    rectSquareGrid,
    TorSquareGrid,
    torSquareGrid,
    -- * Grids with hexagonal tiles
    HexHexGrid,
    hexHexGrid,
    ParaHexGrid,
    paraHexGrid
  ) where

import Data.Eq.Unicode ((≡), (≠))
import Data.Function (on)
import Data.List (groupBy, nub, nubBy, sortBy)
import Data.Ord (comparing)
import Data.Ord.Unicode ((≤), (≥))

-- | A regular arrangement of tiles.
--   Minimal complete definition: @indices@, @distance@ and @size@.
class Eq x ⇒ Grid g s x | g → s, g → x where

  -- | Returns the indices of all tiles in a grid.
  indices ∷ g → [x]

  -- | @'distance' g a b@ returns the minimum number of moves required
  --   to get from the tile at index @a@ to the tile at index @b@ in
  --   grid @g@, moving between adjacent tiles at each step. (Two tiles
  --   are adjacent if they share an edge.) If @a@ or @b@ are not
  --   contained within @g@, the result is undefined.
  distance ∷ g → x → x → Int

  -- | @'minDistance' g bs a@ returns the minimum number of moves 
  --   required to get from any of the tiles at indices @bs@ to the tile
  --   at index @a@ in grid @g@, moving between adjacent tiles at each
  --   step. (Two tiles are adjacent if they share an edge.) If @a@ or
  --   any of @bs@ are not contained within @g@, the result is 
  --   undefined.
  minDistance ∷ g → [x] → x → Int
  minDistance g xs x = minimum . map (distance g x) $ xs

  -- | Returns the dimensions of the grid. 
  --   For example, if @g@ is a 4x3 rectangular grid, @'size' g@ would
  --   return @(4, 3)@, while @'tileCount' g@ would return @12@.
  size ∷ g → s

  -- | @'neighbours' g x@ returns the indices of the tiles in the grid
  --   @g@ which are adjacent to the tile with index @x@.
  neighbours ∷ g → x → [x]
  neighbours g x = filter (\a → distance g x a ≡ 1 ) $ indices g

  -- | @'numNeighbours' g x@ returns the number of tiles in the grid
  --   @g@ which are adjacent to the tile with index @x@.
  numNeighbours ∷ g → x → Int
  numNeighbours g = length . neighbours g

  -- | @g `'contains'` x@ returns @True@ if the index @x@ is contained 
  --   within the grid @g@, otherwise it returns false.
  contains ∷ g → x → Bool
  contains g x = x `elem` indices g

  -- | @'viewpoint' g x@ returns a list of pairs associating the index
  --   of each tile in @g@ with its distance to the tile with index @x@.
  --   If @x@ is not contained within @g@, the result is undefined.
  viewpoint ∷ g → x → [(x, Int)]
  viewpoint g p = map f (indices g)
    where f x = (x, distance g p x)

  -- | Returns the number of tiles in a grid. Compare with @'size'@.
  tileCount ∷ g → Int
  tileCount = length . indices

  -- | Returns @True@ if the number of tiles in a grid is zero, @False@ 
  --   otherwise.
  empty ∷ g → Bool
  empty g = tileCount g ≡ 0

  -- | Returns @False@ if the number of tiles in a grid is zero, @True@ 
  --   otherwise.
  nonEmpty ∷ g → Bool
  nonEmpty = not . empty

  -- | A list of all edges in a grid, where the edges are represented by
  --   a pair of indices of adjacent tiles.
  edges ∷ g → [(x,x)]
  edges g = nubBy sameEdge $ concatMap (`adjacentEdges` g) $ indices g

  -- | @'isAdjacent' g a b@ returns @True@ if the tile at index @a@ is
  --   adjacent to the tile at index @b@ in @g@. (Two tiles are adjacent
  --   if they share an edge.) If @a@ or @b@ are not contained within
  --   @g@, the result is undefined.
  isAdjacent ∷ Grid g s x ⇒ g → x → x → Bool
  isAdjacent g a b = distance g a b ≡ 1

  -- | @'adjacentTilesToward' g a b@ returns the indices of all tiles
  --   which are neighbours of the tile at index @a@, and which are
  --   closer to the tile at @b@ than @a@ is. In other words, it returns
  --   the possible next steps on a minimal path from @a@ to @b@. If @a@
  --   or @b@ are not contained within @g@, or if there is no path from 
  --   @a@ to @b@ (e.g., a disconnected grid), the result is undefined.
  adjacentTilesToward ∷ g → x → x → [x]
  adjacentTilesToward g a b
    | a ≡ b            = []
    | otherwise        = filter f $ neighbours g a
    where f x = distance g x b ≡ distance g a b - 1

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
  minimalPaths ∷ g → x → x → [[x]]
  minimalPaths g a b | a ≡ b              = [[a]]
                     | distance g a b ≡ 1 = [[a,b]]
                     | otherwise          = map (a:) xs
    where xs = concatMap (\x → minimalPaths g x b) ys
          ys = adjacentTilesToward g a b

sameEdge ∷ Eq t ⇒ (t, t) → (t, t) → Bool
sameEdge (a,b) (c,d) = (a,b) ≡ (c,d) || (a,b) ≡ (d,c)

adjacentEdges ∷ Grid g s t ⇒ t → g → [(t, t)]
adjacentEdges i g = map (\j → (i,j)) $ neighbours g i


-- | A regular arrangement of tiles with an edge.
--   Minimal complete definition: @boundary@.
class Grid g s x ⇒ BoundedGrid g s x where
  -- | Returns a the indices of all the tiles at the boundary of a grid, 
  --   including corner tiles.
  boundary ∷ g → [x]

  -- | @'isBoundary' g x@' returns @True@ if the tile with index @x@ is
  --   on a boundary of @g@, @False@ otherwise. (Corner tiles are also
  --   boundary tiles.)
  isBoundary ∷ g → x → Bool
  isBoundary g x = x `elem` boundary g

  -- | Returns the index of the tile(s) that require the maximum number 
  --   of moves to reach the nearest boundary tile. A grid may have more
  --   than one central tile (e.g., a rectangular grid with an even 
  --   number of rows and columns will have four central tiles).
  centre ∷ g → [x]
  centre g = map fst . head . reverse . groupBy ((==) `on` snd) . 
                sortBy (comparing snd) $ xds
    where xds = map (\y -> (y, minDistance g bs y)) $ indices g
          bs = boundary g


  -- | @'isCentre' g x@' returns @True@ if the tile with index @x@ is
  --   a centre tile of @g@, @False@ otherwise.
  isCentre ∷ g → x → Bool
  isCentre g x = x `elem` centre g


--
-- Triangular tiles
--

-- | For triangular tiles, it is convenient to define a third component 
--   z.
triZ ∷ Int → Int → Int            
triZ x y | even y    = -x - y
         | otherwise = -x - y + 1

triDistance ∷ Grid g s (Int, Int) ⇒ g → (Int, Int) → (Int, Int) → Int
triDistance g (x1, y1) (x2, y2) = 
    if g `contains` (x1, y1) && g `contains` (x2, y2)
      then maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
      else undefined
        where z1 = triZ x1 y1
              z2 = triZ x2 y2

triNeighbours ∷ Grid g s (Int, Int) ⇒ g → (Int, Int) → [(Int, Int)]
triNeighbours g (x,y) = filter (g `contains`) xs
    where xs | even y    = [(x-1,y+1), (x+1,y+1), (x+1,y-1)]
             | otherwise = [(x-1,y-1), (x-1,y+1), (x+1,y-1)]

--
-- Triangular grids with triangular tiles
--

-- | A triangular grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TriTriGrid = TriTriGrid Int [(Int, Int)] deriving Eq

instance Show TriTriGrid where 
  show (TriTriGrid s _) = "triTriGrid " ++ show s

instance Grid TriTriGrid Int (Int, Int) where
  indices (TriTriGrid _ xs) = xs
  neighbours = triNeighbours
  distance = triDistance
  contains (TriTriGrid s _) (x, y) = inTriGrid (x,y) s
  size (TriTriGrid s _) = s

inTriGrid ∷ (Int, Int) → Int → Bool
inTriGrid (x, y) s = x ≥ 0 && y ≥ 0 && even (x+y) && abs z ≤ 2*s-2
  where z = triZ x y

instance BoundedGrid TriTriGrid Int (Int, Int) where
--  corners g = if empty g 
--                then [] 
--                else nub [(0,0), (0,2*s-2), (2*s-2, 0)] 
--    where s = size g
  boundary g = west ++ east ++ south
    where s = size g
          west = [(0,k) | k ← [0,2..2*s-2]]
          east = [(k,2*s-2-k) | k ← [2,4..2*s-2]]
          south = [(k,0) | k ← [2*s-4,2*s-6..2]]
  centre g = case s `mod` 3 of
    0 → trefoilWithTop (k-1,k+1) where k = (2*s) `div` 3
    1 → [(k,k)] where k = (2*(s-1)) `div` 3
    2 → [(k+1,k+1)] where k = (2*(s-2)) `div` 3
    _ → error "This will never happen."
    where s = size g

trefoilWithTop ∷ (Int, Int) → [(Int,Int)]
trefoilWithTop (i,j) = [(i,j), (i+2, j-2), (i,j-2)]

-- | @'triTriGrid' s@ returns a triangular grid with sides of 
--   length @s@, using triangular tiles. If @s@ is nonnegative, the 
--   resulting grid will have @s^2@ tiles. Otherwise, the resulting grid
--   will be empty and the list of indices will be null.
triTriGrid ∷ Int → TriTriGrid
triTriGrid s = 
  TriTriGrid s [(xx,yy) | xx ← [0..2*(s-1)], 
                          yy ← [0..2*(s-1)], 
                          (xx,yy) `inTriGrid` s]

--
-- Parallelogrammatical grids with triangular tiles
--

-- | A Parallelogrammatical grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data ParaTriGrid = ParaTriGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show ParaTriGrid where 
  show (ParaTriGrid (r,c) _) = "paraTriGrid " ++ show r ++ " " ++ show c

instance Grid ParaTriGrid (Int, Int) (Int, Int) where
  indices (ParaTriGrid _ xs) = xs
  neighbours = triNeighbours
  distance = triDistance
  size (ParaTriGrid s _) = s

instance BoundedGrid ParaTriGrid (Int, Int) (Int, Int) where
  boundary g = west ++ north ++ east ++ south
    where (r,c) = size g
          west = [(0,k) | k ← [0,2..2*r-2], c>0]
          north = [(k,2*r-1) | k ← [1,3..2*c-1], r>0]
          east = [(2*c-1,k) | k ← [2*r-3,2*r-5..1], c>0]
          south = [(k,0) | k ← [2*c-2,2*c-4..2], r>0]
  centre g = paraTriGridCentre . size $ g

paraTriGridCentre ∷ (Int, Int) → [(Int, Int)]
paraTriGridCentre (r,c)
  | odd r && odd c             = [(c-1,r-1), (c,r)]
  | even r && even c && r == c = bowtie (c-1,r-1)
  | even r && even c && r > c  
      = bowtie (c-1,r-3) ++ bowtie (c-1,r-1) ++ bowtie (c-1,r+1)
  | even r && even c && r < c  
      = bowtie (c-3,r-1) ++ bowtie (c-1,r-1) ++ bowtie (c+1,r-1)
  | otherwise                  = [(c-1,r), (c,r-1)]

bowtie :: (Int,Int) -> [(Int,Int)]
bowtie (i,j) = [(i,j), (i+1,j+1)]

-- | @'paraTriGrid' r c@ returns a grid in the shape of a 
--   parallelogram with @r@ rows and @c@ columns, using triangular 
--   tiles. If @r@ and @c@ are both nonnegative, the resulting grid will
--   have @2*r*c@ tiles. Otherwise, the resulting grid will be empty and
--   the list of indices will be null.
paraTriGrid ∷ Int → Int → ParaTriGrid
paraTriGrid r c = 
  ParaTriGrid (r,c) [(x,y) | x ← [0..2*c-1], y ← [0..2*r-1], even (x+y)]

--
-- Rectangular grids with square tiles
--

-- | A rectangular grid with square tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data RectSquareGrid = RectSquareGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show RectSquareGrid where 
  show (RectSquareGrid (r,c) _) = 
    "rectSquareGrid " ++ show r ++ " " ++ show c

instance Grid RectSquareGrid (Int, Int) (Int, Int) where
  indices (RectSquareGrid _ xs) = xs
  neighbours g (x, y) = 
    filter (g `contains`) [(x-1,y), (x,y+1), (x+1,y), (x,y-1)]
  distance g (x1, y1) (x2, y2) = 
    if g `contains` (x1, y1) && g `contains` (x2, y2)
      then abs (x2-x1) + abs (y2-y1)
      else undefined
  size (RectSquareGrid s _) = s
  adjacentTilesToward g a@(x1, y1) (x2, y2) = 
    filter (\i → g `contains` i && i ≠ a) $ nub [(x1,y1+dy),(x1+dx,y1)]
      where dx = signum (x2-x1)
            dy = signum (y2-y1)

instance BoundedGrid RectSquareGrid (Int, Int) (Int, Int) where
  boundary g = cartesianIndices . size $ g
  centre g = cartesianCentre . size $ g

cartesianIndices
  ∷ (Enum r, Enum c, Num r, Num c, Ord r, Ord c) ⇒
     (r, c) → [(c, r)]
cartesianIndices (r, c) = west ++ north ++ east ++ south
  where west = [(0,k) | k ← [0,1..r-1], c>0]
        north = [(k,r-1) | k ← [1,2..c-1], r>0]
        east = [(c-1,k) | k ← [r-2,r-3..0], c>1]
        south = [(k,0) | k ← [c-2,c-3..1], r>1]

cartesianCentre ∷ (Int, Int) → [(Int, Int)]
cartesianCentre (r,c) = [(i,j) | i ← midpoints c, j ← midpoints r]

midpoints ∷ Int → [Int]
midpoints k = if even k then [m-1,m] else [m]
  where m = floor (k'/2.0)
        k' = fromIntegral k ∷ Double

-- | @'rectSquareGrid' r c@ produces a rectangular grid with @r@ rows
--   and @c@ columns, using square tiles. If @r@ and @c@ are both 
--   nonnegative, the resulting grid will have @r*c@ tiles. Otherwise, 
--   the resulting grid will be empty and the list of indices will be 
--   null.
rectSquareGrid ∷ Int → Int → RectSquareGrid
rectSquareGrid r c = 
  RectSquareGrid (r,c) [(x,y) | x ← [0..c-1], y ← [0..r-1]]

--
-- Toroidal grids with square tiles.
--

-- | A toroidal grid with square tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TorSquareGrid = TorSquareGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show TorSquareGrid where 
  show (TorSquareGrid (r,c) _) = "torSquareGrid " ++ show r ++ " " ++ show c

instance Grid TorSquareGrid (Int, Int) (Int, Int) where
  indices (TorSquareGrid _ xs) = xs
  neighbours (TorSquareGrid (r,c) _) (x,y) = 
    nub $ filter (\(xx,yy) → xx ≠ x || yy ≠ y) 
      [((x-1) `mod` c,y), (x,(y+1) `mod` r), ((x+1) `mod` c,y), 
        (x,(y-1) `mod` r)]
  distance g@(TorSquareGrid (r,c) _) (x1, y1) (x2, y2) =
    if g `contains` (x1, y1) && g `contains` (x2, y2)
      then min adx (abs (c-adx)) + min ady (abs (r-ady))
      else undefined 
    where adx = abs (x2 - x1)
          ady = abs (y2 - y1)
  size (TorSquareGrid s _) = s

-- | @'torSquareGrid' r c@ returns a toroidal grid with @r@ 
--   rows and @c@ columns, using square tiles. If @r@ and @c@ are 
--   both nonnegative, the resulting grid will have @r*c@ tiles. Otherwise, 
--   the resulting grid will be empty and the list of indices will be null.
torSquareGrid ∷ Int → Int → TorSquareGrid
torSquareGrid r c = TorSquareGrid (r,c) [(x, y) | x ← [0..c-1], y ← [0..r-1]]

--
-- Hexagonal tiles
--

hexDistance ∷ Grid g s (Int, Int) ⇒ g → (Int, Int) → (Int, Int) → Int
hexDistance g (x1, y1) (x2, y2) = 
  if g `contains` (x1, y1) && g `contains` (x2, y2)
    then maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
    else undefined
  where z1 = -x1 - y1
        z2 = -x2 - y2

--
-- Hexagonal grids with hexagonal tiles
--

-- | A hexagonal grid with hexagonal tiles
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data HexHexGrid = HexHexGrid Int [(Int, Int)] deriving Eq

instance Show HexHexGrid where show (HexHexGrid s _) = "hexHexGrid " ++ show s

instance Grid HexHexGrid Int (Int, Int) where
  indices (HexHexGrid _ xs) = xs
  neighbours g (x,y) = filter (g `contains`) 
    [(x-1,y), (x-1,y+1), (x,y+1), (x+1,y), (x+1,y-1), (x,y-1)]
  distance = hexDistance
  size (HexHexGrid s _) = s

instance BoundedGrid HexHexGrid Int (Int, Int) where
  boundary g = 
    north ++ northeast ++ southeast ++ south ++ southwest ++ northwest
    where s = size g
          north = [(k,s-1) | k ← [-s+1,-s+2..0]]
          northeast = [(k,s-1-k) | k ← [1,2..s-1]]
          southeast = [(s-1,k) | k ← [-1,-2..(-s)+1]]
          south = [(k,(-s)+1) | k ← [s-2,s-3..0]]
          southwest = [(k,(-s)+1-k) | k ← [-1,-2..(-s)+1]]
          northwest = [(-s+1,k) | k ← [1,2..s-2]]
  centre _ = [(0,0)]

-- | @'hexHexGrid' s@ returns a grid of hexagonal shape, with
--   sides of length @s@, using hexagonal tiles. If @s@ is nonnegative, the 
--   resulting grid will have @3*s*(s-1) + 1@ tiles. Otherwise, the resulting 
--   grid will be empty and the list of indices will be null.
hexHexGrid ∷ Int → HexHexGrid
hexHexGrid r = HexHexGrid r [(x, y) | x ← [-r+1..r-1], y ← f x]
  where f x = if x < 0 then [1-r-x .. r-1] else [1-r .. r-1-x]

--
-- Parallelogrammatical grids with hexagonal tiles
--

-- | A parallelogramatical grid with hexagonal tiles
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data ParaHexGrid = ParaHexGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show ParaHexGrid where 
  show (ParaHexGrid (r,c) _) = "paraHexGrid " ++ show r ++ " " ++ show c

instance Grid ParaHexGrid (Int, Int) (Int, Int) where
  indices (ParaHexGrid _ xs) = xs
  neighbours g (x,y) = filter (g `contains`) 
    [(x-1,y), (x-1,y+1), (x,y+1), (x+1,y), (x+1,y-1), (x,y-1)]
  distance = hexDistance
  size (ParaHexGrid s _) = s

instance BoundedGrid ParaHexGrid (Int, Int) (Int, Int) where
  boundary g = cartesianIndices . size $ g
  centre g = cartesianCentre . size $ g

-- | @'paraHexGrid' r c@ returns a grid in the shape of a 
--   parallelogram with @r@ rows and @c@ columns, using hexagonal tiles. If 
--   @r@ and @c@ are both nonnegative, the resulting grid will have @r*c@ tiles.
--   Otherwise, the resulting grid will be empty and the list of indices will 
--   be null.
paraHexGrid ∷ Int → Int → ParaHexGrid
paraHexGrid r c = 
  ParaHexGrid (r,c) [(x, y) | x ← [0..c-1], y ← [0..r-1]]


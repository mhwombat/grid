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
{-# LANGUAGE UnicodeSyntax, TypeFamilies, FlexibleContexts #-}

module Math.Geometry.GridInternal
  (
    -- * Generic
    Grid(..),
    FiniteGrid(..),
    BoundedGrid(..),
    WrappedGrid(..),
    -- * Grids with triangular tiles
    UnboundedTriGrid,
    TriTriGrid,
    triTriGrid,
    ParaTriGrid,
    paraTriGrid,
    RectTriGrid,
    rectTriGrid,
    TorTriGrid,
    torTriGrid,
    -- * Grids with square tiles
    UnboundedSquareGrid,
    RectSquareGrid,
    rectSquareGrid,
    TorSquareGrid,
    torSquareGrid,
    -- * Grids with hexagonal tiles
    UnboundedHexGrid,
    HexHexGrid,
    hexHexGrid,
    ParaHexGrid,
    paraHexGrid
  ) where

import Prelude hiding (null)

import Data.Eq.Unicode ((≡), (≠))
import Data.Function (on)
import Data.List (groupBy, nub, nubBy, sortBy)
import Data.Ord (comparing)
import Data.Ord.Unicode ((≤), (≥))

-- | A regular arrangement of tiles.
--   Minimal complete definition: @indices@ and @distance@.
class Grid g where
  type Index g

  -- | Returns the indices of all tiles in a grid.
  indices ∷ g → [Index g]

  -- | @'distance' g a b@ returns the minimum number of moves required
  --   to get from the tile at index @a@ to the tile at index @b@ in
  --   grid @g@, moving between adjacent tiles at each step. (Two tiles
  --   are adjacent if they share an edge.) If @a@ or @b@ are not
  --   contained within @g@, the result is undefined.
  distance ∷ g → Index g → Index g → Int

  -- | @'minDistance' g bs a@ returns the minimum number of moves 
  --   required to get from any of the tiles at indices @bs@ to the tile
  --   at index @a@ in grid @g@, moving between adjacent tiles at each
  --   step. (Two tiles are adjacent if they share an edge.) If @a@ or
  --   any of @bs@ are not contained within @g@, the result is 
  --   undefined.
  minDistance ∷ g → [Index g] → Index g → Int
  minDistance g xs x = minimum . map (distance g x) $ xs

  -- | @'neighbours' g x@ returns the indices of the tiles in the grid
  --   @g@ which are adjacent to the tile with index @x@.
  neighbours ∷ g → Index g → [Index g]
  neighbours g x = filter (\a → distance g x a ≡ 1 ) $ indices g

  -- | @'numNeighbours' g x@ returns the number of tiles in the grid
  --   @g@ which are adjacent to the tile with index @x@.
  numNeighbours ∷ g → Index g → Int
  numNeighbours g = length . neighbours g

  -- | @g `'contains'` x@ returns @True@ if the index @x@ is contained 
  --   within the grid @g@, otherwise it returns false.
  contains ∷ Eq (Index g) ⇒ g → Index g → Bool
  contains g x = x `elem` indices g

  -- | @'viewpoint' g x@ returns a list of pairs associating the index
  --   of each tile in @g@ with its distance to the tile with index @x@.
  --   If @x@ is not contained within @g@, the result is undefined.
  viewpoint ∷ g → Index g → [(Index g, Int)]
  viewpoint g p = map f (indices g)
    where f x = (x, distance g p x)

  -- | Returns the number of tiles in a grid. Compare with @'size'@.
  tileCount ∷ g → Int
  tileCount = length . indices

  -- | Returns @True@ if the number of tiles in a grid is zero, @False@ 
  --   otherwise.
  null ∷ g → Bool
  null g = tileCount g ≡ 0

  -- | Returns @False@ if the number of tiles in a grid is zero, @True@ 
  --   otherwise.
  nonNull ∷ g → Bool
  nonNull = not . null

  -- | A list of all edges in a grid, where the edges are represented by
  --   a pair of indices of adjacent tiles.
  edges ∷ Eq (Index g) ⇒ g → [(Index g,Index g)]
  edges g = nubBy sameEdge $ concatMap (`adjacentEdges` g) $ indices g

  -- | @'isAdjacent' g a b@ returns @True@ if the tile at index @a@ is
  --   adjacent to the tile at index @b@ in @g@. (Two tiles are adjacent
  --   if they share an edge.) If @a@ or @b@ are not contained within
  --   @g@, the result is undefined.
  isAdjacent ∷ Eq (Index g) ⇒ g → Index g → Index g → Bool
  isAdjacent g a b = a `elem` (neighbours g b)

  -- | @'adjacentTilesToward' g a b@ returns the indices of all tiles
  --   which are neighbours of the tile at index @a@, and which are
  --   closer to the tile at @b@ than @a@ is. In other words, it returns
  --   the possible next steps on a minimal path from @a@ to @b@. If @a@
  --   or @b@ are not contained within @g@, or if there is no path from 
  --   @a@ to @b@ (e.g., a disconnected grid), the result is undefined.
  adjacentTilesToward ∷ g → Index g → Index g → [Index g]
  adjacentTilesToward g a b = filter f $ neighbours g a
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
  minimalPaths ∷ Eq (Index g) ⇒ g → Index g → Index g → [[Index g]]
  minimalPaths g a b | a ≡ b              = [[a]]
                     | distance g a b ≡ 1 = [[a,b]]
                     | otherwise          = map (a:) xs
    where xs = concatMap (\x → minimalPaths g x b) ys
          ys = adjacentTilesToward g a b

sameEdge ∷ Eq t ⇒ (t, t) → (t, t) → Bool
sameEdge (a,b) (c,d) = (a,b) ≡ (c,d) || (a,b) ≡ (d,c)

adjacentEdges ∷ Grid g ⇒ Index g → g → [(Index g, Index g)]
adjacentEdges i g = map (\j → (i,j)) $ neighbours g i


-- | A regular arrangement of tiles where the number of tiles is finite.
--   Minimal complete definition: @size@.
class Grid g ⇒ FiniteGrid g where
  type Size s
  -- | Returns the dimensions of the grid. 
  --   For example, if @g@ is a 4x3 rectangular grid, @'size' g@ would
  --   return @(4, 3)@, while @'tileCount' g@ would return @12@.
  size ∷ g → Size g


-- | A regular arrangement of tiles with an edge.
--   Minimal complete definition: @tileSideCount@.
class Grid g ⇒ BoundedGrid g where
  -- | Returns the number of sides a tile has
  tileSideCount ∷ g → Int

  -- | Returns a the indices of all the tiles at the boundary of a grid.
  boundary ∷ g → [Index g]
  boundary g = map fst . filter f $ xds
    where xds = map (\y → (y, numNeighbours g y)) $ indices g
          f (_,n) = n < tileSideCount g 


  -- | @'isBoundary' g x@' returns @True@ if the tile with index @x@ is
  --   on a boundary of @g@, @False@ otherwise. (Corner tiles are also
  --   boundary tiles.)
  isBoundary ∷ Eq (Index g) ⇒ g → Index g → Bool
  isBoundary g x = x `elem` boundary g

  -- | Returns the index of the tile(s) that require the maximum number 
  --   of moves to reach the nearest boundary tile. A grid may have more
  --   than one central tile (e.g., a rectangular grid with an even 
  --   number of rows and columns will have four central tiles).
  centre ∷ g → [Index g]
  centre g = map fst . head . reverse . groupBy ((≡) `on` snd) . 
                sortBy (comparing snd) $ xds
    where xds = map (\y → (y, minDistance g bs y)) $ indices g
          bs = boundary g


  -- | @'isCentre' g x@' returns @True@ if the tile with index @x@ is
  --   a centre tile of @g@, @False@ otherwise.
  isCentre ∷ Eq (Index g) ⇒ g → Index g → Bool
  isCentre g x = x `elem` centre g

class (Grid g) ⇒ WrappedGrid g where
  normalise ∷ g → Index g → Index g

-- Calculate the neighbours of a tile in a bounded grid by as we would 
-- in an unbounded grid, but then filter out the tiles that are not in
-- bounds.
neighboursBasedOn
  ∷ (Eq (Index g), Grid u, Grid g, Index u ~ Index g) ⇒
     g → u → Index g → [Index g]
neighboursBasedOn u g = filter (g `contains`) . neighbours u

-- Calculate the distance between two tiles in a bounded grid by as we 
-- would in an unbounded grid, but only if both tiles are in bounds.
distanceBasedOn
  ∷ (Eq (Index g), Grid u, Grid g, Index u ~ Index g) ⇒
     g → u → Index g → Index g → Int
distanceBasedOn u g a b = 
  if g `contains` a && g `contains` b
    then distance u a b
    else undefined

--
-- Triangular tiles
--

data UnboundedTriGrid = UnboundedTriGrid deriving Show

instance Grid UnboundedTriGrid where
  type Index UnboundedTriGrid = (Int, Int)
  indices _ = undefined
  neighbours _ (x,y) = if even y
                         then [(x-1,y+1), (x+1,y+1), (x+1,y-1)]
                         else [(x-1,y-1), (x-1,y+1), (x+1,y-1)]
  distance _ (x1, y1) (x2, y2) = 
    maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
      where z1 = triZ x1 y1
            z2 = triZ x2 y2
  contains _ _ = True

-- | For triangular tiles, it is convenient to define a third component 
--   z.
triZ ∷ Int → Int → Int            
triZ x y = if even y then -x - y else -x - y + 1

--
-- Triangular grids with triangular tiles
--

-- | A triangular grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TriTriGrid = TriTriGrid Int [(Int, Int)] deriving Eq

instance Show TriTriGrid where 
  show (TriTriGrid s _) = "triTriGrid " ++ show s

instance Grid TriTriGrid where
  type Index TriTriGrid = (Int, Int)
  indices (TriTriGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedTriGrid
  distance = distanceBasedOn UnboundedTriGrid
  contains (TriTriGrid s _) (x, y) = inTriTriGrid (x,y) s

inTriTriGrid ∷ (Int, Int) → Int → Bool
inTriTriGrid (x, y) s = x ≥ 0 && y ≥ 0 && even (x+y) && abs z ≤ 2*s-2
  where z = triZ x y

instance FiniteGrid TriTriGrid where
  type Size TriTriGrid = Int
  size (TriTriGrid s _) = s

instance BoundedGrid TriTriGrid where
  tileSideCount _ = 3
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
          trefoilWithTop (i,j) = [(i,j), (i+2, j-2), (i,j-2)]

-- | @'triTriGrid' s@ returns a triangular grid with sides of 
--   length @s@, using triangular tiles. If @s@ is nonnegative, the 
--   resulting grid will have @s^2@ tiles. Otherwise, the resulting grid
--   will be null and the list of indices will be null.
triTriGrid ∷ Int → TriTriGrid
triTriGrid s = 
  TriTriGrid s [(xx,yy) | xx ← [0..2*(s-1)], 
                          yy ← [0..2*(s-1)], 
                          (xx,yy) `inTriTriGrid` s]

--
-- Parallelogrammatical grids with triangular tiles
--

-- | A Parallelogrammatical grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data ParaTriGrid = ParaTriGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show ParaTriGrid where 
  show (ParaTriGrid (r,c) _) = "paraTriGrid " ++ show r ++ " " ++ show c

instance Grid ParaTriGrid where
  type Index ParaTriGrid = (Int, Int)
  indices (ParaTriGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedTriGrid
  distance = distanceBasedOn UnboundedTriGrid

instance FiniteGrid ParaTriGrid where
  type Size ParaTriGrid = (Int, Int)
  size (ParaTriGrid s _) = s

instance BoundedGrid ParaTriGrid where
  tileSideCount _ = 3
  boundary g = west ++ north ++ east ++ south
    where (r,c) = size g
          west = [(0,k) | k ← [0,2..2*r-2], c>0]
          north = [(k,2*r-1) | k ← [1,3..2*c-1], r>0]
          east = [(2*c-1,k) | k ← [2*r-3,2*r-5..1], c>0]
          south = [(k,0) | k ← [2*c-2,2*c-4..2], r>0]
  centre g = f . size $ g
    where f (r,c)
            | odd r && odd c             
                = [(c-1,r-1), (c,r)]
            | even r && even c && r ≡ c 
                = bowtie (c-1,r-1)
            | even r && even c && r > c  
                = bowtie (c-1,r-3) ++ bowtie (c-1,r-1) ++ bowtie (c-1,r+1)
            | even r && even c && r < c  
                = bowtie (c-3,r-1) ++ bowtie (c-1,r-1) ++ bowtie (c+1,r-1)
            | otherwise                  
                = [(c-1,r), (c,r-1)]
          bowtie (i,j) = [(i,j), (i+1,j+1)]

-- | @'paraTriGrid' r c@ returns a grid in the shape of a 
--   parallelogram with @r@ rows and @c@ columns, using triangular 
--   tiles. If @r@ and @c@ are both nonnegative, the resulting grid will
--   have @2*r*c@ tiles. Otherwise, the resulting grid will be null and
--   the list of indices will be null.
paraTriGrid ∷ Int → Int → ParaTriGrid
paraTriGrid r c = 
  ParaTriGrid (r,c) [(x,y) | x ← [0..2*c-1], y ← [0..2*r-1], even (x+y)]


--
-- Rectangular grids with triangular tiles
--

-- | A rectangular grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data RectTriGrid = RectTriGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show RectTriGrid where 
  show (RectTriGrid (r,c) _) = "rectTriGrid " ++ show r ++ " " ++ show c

instance Grid RectTriGrid where
  type Index RectTriGrid = (Int, Int)
  indices (RectTriGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedTriGrid
  distance = distanceBasedOn UnboundedTriGrid

instance FiniteGrid RectTriGrid where
  type Size RectTriGrid = (Int, Int)
  size (RectTriGrid s _) = s

instance BoundedGrid RectTriGrid where
  tileSideCount _ = 3

-- | @'rectTriGrid' r c@ returns a grid in the shape of a 
--   rectangle (with jagged edges) that has @r@ rows and @c@ columns, 
--   using triangular tiles. If @r@ and @c@ are both nonnegative, the 
--   resulting grid will have @2*r*c@ tiles. Otherwise, the resulting grid will be null and
--   the list of indices will be null.
rectTriGrid ∷ Int → Int → RectTriGrid
rectTriGrid r c = RectTriGrid (r,c) [(x,y) | y ← [0..2*r-1], x ← [xMin y .. xMax c y], even (x+y)]
  where xMin y = if even y then w else w+1
          where w = -2*((y+1) `div` 4)
        xMax c2 y = xMin y + 2*(c2-1)


--
-- Toroidal grids with triangular tiles
--

-- | A toroidal grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TorTriGrid = TorTriGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show TorTriGrid where 
  show (TorTriGrid (r,c) _) = "torTriGrid " ++ show r ++ " " ++ show c

instance Grid TorTriGrid where
  type Index TorTriGrid = (Int, Int)
  indices (TorTriGrid _ xs) = xs
  neighbours g = nub . map (normalise g) . neighbours UnboundedTriGrid
  distance g (xa, ya) (xb, yb) = 
    if g `contains` (xa, ya) && g `contains` (xb, yb)
      then minimum [distance UnboundedTriGrid (xa, ya) (xb, yb),
                    distance UnboundedTriGrid (xa, ya) (xb + 2*c, yb),
                    distance UnboundedTriGrid (xa, ya) (xb - r, yb + 2*r),
                    distance UnboundedTriGrid (xa, ya) (xb, yb),
                    distance UnboundedTriGrid (xa + 2*c, ya) (xb, yb),
                    distance UnboundedTriGrid (xa - r, ya + 2*r) (xb, yb)]
      else undefined
    where (r,c) = size g

xMinTorTri ∷ Int → Int
xMinTorTri y = if even y then w else w+1
  where w = -2*((y+1) `div` 4)


instance FiniteGrid TorTriGrid where
  type Size TorTriGrid = (Int, Int)
  size (TorTriGrid s _) = s

instance WrappedGrid TorTriGrid where
  normalise g (x,y)
    | y < 0            = normalise g (x-r,y+2*r)
    | y > 2*r-1        = normalise g (x+r,y-2*r)
    | x < xMin         = normalise g (x+2*c,y)
    | x > xMin + 2*c-1 = normalise g (x-2*c,y)
    | otherwise        = (x,y)
    where xMin = xMinTorTri y
          (r, c) = size g

-- | @'torTriGrid' r c@ returns a toroidal grid with @r@ rows and @c@ 
--   columns, using triangular tiles. If @r@ is odd, the result is
--   undefined because the grid edges would overlap. If @r@ and @c@  
--   are both nonnegative, the resulting grid will have @2*r*c@ tiles. 
--   Otherwise, the resulting grid will be null and the list of indices
--   will be null.
torTriGrid ∷ Int → Int → TorTriGrid
torTriGrid r c = 
  if even r
    then TorTriGrid (r,c) [(x,y) | y ← [0..2*r-1], 
                                   x ← [xMinTorTri y .. xMax c y], 
                                   even (x+y)]
    else undefined
  where xMax c2 y = xMinTorTri y + 2*(c2-1)


--
-- Square tiles
--

data UnboundedSquareGrid = UnboundedSquareGrid deriving Show

instance Grid UnboundedSquareGrid where
  type Index UnboundedSquareGrid = (Int, Int)
  indices _ = undefined
  neighbours _ (x,y) = [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]
  distance _ (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)
  contains _ _ = True

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

instance Grid RectSquareGrid where
  type Index RectSquareGrid = (Int, Int)
  indices (RectSquareGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedSquareGrid
  distance = distanceBasedOn UnboundedSquareGrid
  adjacentTilesToward g a@(x1, y1) (x2, y2) = 
    filter (\i → g `contains` i && i ≠ a) $ nub [(x1,y1+dy),(x1+dx,y1)]
      where dx = signum (x2-x1)
            dy = signum (y2-y1)

instance FiniteGrid RectSquareGrid where
  type Size RectSquareGrid = (Int, Int)
  size (RectSquareGrid s _) = s

instance BoundedGrid RectSquareGrid where
  tileSideCount _ = 4
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
--   the resulting grid will be null and the list of indices will be 
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

instance Grid TorSquareGrid where
  type Index TorSquareGrid = (Int, Int)
  indices (TorSquareGrid _ xs) = xs
--  neighbours (TorSquareGrid (r,c) _) (x,y) = 
--    nub $ filter (\(xx,yy) → xx ≠ x || yy ≠ y) 
--      [((x-1) `mod` c,y), (x,(y+1) `mod` r), ((x+1) `mod` c,y), 
--        (x,(y-1) `mod` r)]
  neighbours g = nub . map (normalise g) . neighbours UnboundedSquareGrid
  distance g@(TorSquareGrid (r,c) _) (x1, y1) (x2, y2) =
    if g `contains` (x1, y1) && g `contains` (x2, y2)
      then min adx (abs (c-adx)) + min ady (abs (r-ady))
      else undefined 
    where adx = abs (x2 - x1)
          ady = abs (y2 - y1)

instance FiniteGrid TorSquareGrid where
  type Size TorSquareGrid = (Int, Int)
  size (TorSquareGrid s _) = s

instance WrappedGrid TorSquareGrid where
  normalise g (x,y) = (x `mod` c, y `mod` r)
    where (r, c) = size g

-- | @'torSquareGrid' r c@ returns a toroidal grid with @r@ 
--   rows and @c@ columns, using square tiles. If @r@ and @c@ are 
--   both nonnegative, the resulting grid will have @r*c@ tiles. Otherwise, 
--   the resulting grid will be null and the list of indices will be null.
torSquareGrid ∷ Int → Int → TorSquareGrid
torSquareGrid r c = TorSquareGrid (r,c) [(x, y) | x ← [0..c-1], y ← [0..r-1]]

--
-- Hexagonal tiles
--

data UnboundedHexGrid = UnboundedHexGrid deriving Show

instance Grid UnboundedHexGrid where
  type Index UnboundedHexGrid = (Int, Int)
  indices _ = undefined
  neighbours _ (x,y) = 
    [(x-1,y), (x-1,y+1), (x,y+1), (x+1,y), (x+1,y-1), (x,y-1)]
  distance _ (x1, y1) (x2, y2) = 
    maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
    where z1 = -x1 - y1
          z2 = -x2 - y2
  contains _ _ = True

--
-- Hexagonal grids with hexagonal tiles
--

-- | A hexagonal grid with hexagonal tiles
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data HexHexGrid = HexHexGrid Int [(Int, Int)] deriving Eq

instance Show HexHexGrid where show (HexHexGrid s _) = "hexHexGrid " ++ show s

instance Grid HexHexGrid where
  type Index HexHexGrid = (Int, Int)
  indices (HexHexGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedHexGrid
  distance = distanceBasedOn UnboundedHexGrid

instance FiniteGrid HexHexGrid where
  type Size HexHexGrid = Int
  size (HexHexGrid s _) = s

instance BoundedGrid HexHexGrid where
  tileSideCount _ = 6
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
--   grid will be null and the list of indices will be null.
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

instance Grid ParaHexGrid where
  type Index ParaHexGrid = (Int, Int)
  indices (ParaHexGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedHexGrid
  distance = distanceBasedOn UnboundedHexGrid

instance FiniteGrid ParaHexGrid where
  type Size ParaHexGrid = (Int, Int)
  size (ParaHexGrid s _) = s

instance BoundedGrid ParaHexGrid where
  tileSideCount _ = 6
  boundary g = cartesianIndices . size $ g
  centre g = cartesianCentre . size $ g

-- | @'paraHexGrid' r c@ returns a grid in the shape of a 
--   parallelogram with @r@ rows and @c@ columns, using hexagonal tiles. If 
--   @r@ and @c@ are both nonnegative, the resulting grid will have @r*c@ tiles.
--   Otherwise, the resulting grid will be null and the list of indices will 
--   be null.
paraHexGrid ∷ Int → Int → ParaHexGrid
paraHexGrid r c = 
  ParaHexGrid (r,c) [(x, y) | x ← [0..c-1], y ← [0..r-1]]


-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FunctionalDependencies, 
    TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Math.Geometry.GridInternal
  (
    -- * Generic
    Grid(..),
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

import Data.Eq.Unicode ((≡))
import Data.List (nub, nubBy)
import Data.Ord.Unicode ((≤), (≥))

-- | A regular arrangement of tiles.
--   Minimal complete definition: @indices@, @distance@, and @size@.
class Eq x ⇒ Grid g s x | g → s, g → x where
  -- | Returns the indices of all tiles in a grid.
  indices ∷ g → [x]
  -- | @'distance' a b@ returns the minimum number of moves required to get
  --   from @a@ to @b@, moving between adjacent tiles at each step. (Two tiles
  --   are adjacent if they share an edge.) If @a@ or @b@ are not contained
  --   within @g@, the result is undefined.
  distance ∷ x → x → g → Int
  -- | Returns the dimensions of the grid. 
  --   For example, if @g@ is a 4x3 rectangular grid, @'size' g@ would return 
  --   @(4, 3)@, while @'tileCount' g@ would return @12@.
  size ∷ g → s
  -- | @'neighbours' x g@ returns the indices of the tiles in the grid @g@
  --   which are adjacent to the tile at @x@.
  neighbours ∷ x → g → [x]
  neighbours x g = filter (\a -> distance x a g ≡ 1 ) $ indices g
  -- | @x `'inGrid'` g@ returns true if the index @x@ is contained within @g@,
  --   otherwise it returns false.
  inGrid ∷ x → g → Bool
  inGrid x g = x `elem` indices g
  -- | @'viewpoint' x g@ returns a list of pairs associating the index of each
  --   tile in @g@ with its distance to the tile with index @x@. If @x@ is not
  --   contained within @g@, the result is undefined.
  viewpoint ∷ x → g → [(x, Int)]
  viewpoint p g = map f (indices g)
    where f x = (x, distance p x g)
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
  -- | A list of all edges in a Grid, where the edges are represented by a
  --   pair of adjacent tiles.
  edges ∷ g → [(x,x)]
  edges g = nubBy sameEdge $ concatMap (`adjacentEdges` g) $ indices g

sameEdge :: Eq t => (t, t) -> (t, t) -> Bool
sameEdge (a,b) (c,d) = (a,b) == (c,d) || (a,b) == (d,c)

adjacentEdges :: Grid g s t => t -> g -> [(t, t)]
adjacentEdges i g = map (\j -> (i,j)) $ i `neighbours` g

--
-- Triangular tiles
--

-- | For triangular tiles, it is convenient to define a third component z.
triZ ∷ Int → Int → Int            
triZ x y | even y    = -x - y
         | otherwise = -x - y + 1

triDistance ∷ Grid g s (Int, Int) ⇒ (Int, Int) → (Int, Int) → g → Int
triDistance (x1, y1) (x2, y2) g = 
    if (x1, y1) `inGrid` g && (x2, y2) `inGrid` g
      then maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
      else undefined
        where z1 = triZ x1 y1
              z2 = triZ x2 y2

triNeighbours :: Grid g s (Int, Int) ⇒ (Int, Int) → g → [(Int, Int)]
triNeighbours (x,y) g = filter (`inGrid` g) xs
    where xs | even y    = [(x-1,y+1), (x+1,y+1), (x+1,y-1)]
             | otherwise = [(x-1,y-1), (x-1,y+1), (x+1,y-1)]

--
-- Triangular grids with triangular tiles
--

-- | A triangular grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TriTriGrid = TriTriGrid Int [(Int, Int)] deriving Eq

instance Show TriTriGrid where show (TriTriGrid s _) = "triTriGrid " ++ show s

instance Grid TriTriGrid Int (Int, Int) where
  indices (TriTriGrid _ xs) = xs
  neighbours = triNeighbours
  distance = triDistance
  inGrid (x, y) (TriTriGrid s _) = inTriGrid (x,y) s
  size (TriTriGrid s _) = s

inTriGrid ∷ (Int, Int) → Int → Bool
inTriGrid (x, y) s = x ≥ 0 && y ≥ 0 && even (x+y) && abs z ≤ 2*s-2
  where z = triZ x y

-- | @'triTriGrid' s@ returns a triangular grid with sides of 
--   length @s@, using triangular tiles. If @s@ is nonnegative, the resulting 
--   grid will have @s^2@ tiles. Otherwise, the resulting grid will be empty 
--   and the list of indices will be null.
triTriGrid ∷ Int → TriTriGrid
triTriGrid s = 
  TriTriGrid s [(xx,yy) | xx ← [0..2*(s-1)], 
                          yy ← [0..2*(s-1)], 
                          inTriGrid (xx,yy) s]

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

-- | @'paraTriGrid' r c@ returns a grid in the shape of a 
--   parallelogram with @r@ rows and @c@ columns, using triangular tiles. 
--   If @r@ and @c@ are both nonnegative, the resulting grid will have @2*r*c@
--   tiles. Otherwise, the resulting grid will be empty and the list of indices
--   will be null.
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
  show (RectSquareGrid (r,c) _) = "rectSquareGrid " ++ show r ++ " " ++ show c

instance Grid RectSquareGrid (Int, Int) (Int, Int) where
  indices (RectSquareGrid _ xs) = xs
  neighbours (x, y) g = filter (`inGrid` g) [(x-1,y), (x,y+1), (x+1,y), (x,y-1)]
  distance (x1, y1) (x2, y2) g = 
    if (x1, y1) `inGrid` g && (x2, y2) `inGrid` g
      then abs (x2-x1) + abs (y2-y1)
      else undefined
  size (RectSquareGrid s _) = s

-- | @'rectSquareGrid' r c@ produces a rectangular grid with @r@ rows and @c@ 
--   columns, using square tiles. If @r@ and @c@ are both nonnegative, the 
--   resulting grid will have @r*c@ tiles. Otherwise, the resulting grid will 
--   be empty and the list of indices will be null.
rectSquareGrid ∷ Int → Int → RectSquareGrid
rectSquareGrid r c = RectSquareGrid (r,c) [(x,y) | x ← [0..c-1], y ← [0..r-1]]

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
  neighbours (x,y) (TorSquareGrid (r,c) _) = 
    nub $ filter (\(xx,yy) → xx /= x || yy /= y) 
      [((x-1) `mod` c,y), (x,(y+1) `mod` r), ((x+1) `mod` c,y), 
        (x,(y-1) `mod` r)]
  distance (x1, y1) (x2, y2) g@(TorSquareGrid (r,c) _) =
    if (x1, y1) `inGrid` g && (x2, y2) `inGrid` g
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

hexDistance ∷ Grid g s (Int, Int) ⇒ (Int, Int) → (Int, Int) → g → Int
hexDistance (x1, y1) (x2, y2) g = 
  if (x1, y1) `inGrid` g && (x2, y2) `inGrid` g
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
  neighbours (x,y) g = filter (`inGrid` g) 
    [(x-1,y), (x-1,y+1), (x,y+1), (x+1,y), (x+1,y-1), (x,y-1)]
  distance = hexDistance
  size (HexHexGrid s _) = s

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
  neighbours (x,y) g = filter (`inGrid` g) 
    [(x-1,y), (x-1,y+1), (x,y+1), (x+1,y), (x+1,y-1), (x,y-1)]
  distance = hexDistance
  size (ParaHexGrid s _) = s

-- | @'paraHexGrid' r c@ returns a grid in the shape of a 
--   parallelogram with @r@ rows and @c@ columns, using hexagonal tiles. If 
--   @r@ and @c@ are both nonnegative, the resulting grid will have @r*c@ tiles.
--   Otherwise, the resulting grid will be empty and the list of indices will 
--   be null.
paraHexGrid ∷ Int → Int → ParaHexGrid
paraHexGrid r c = 
  ParaHexGrid (r,c) [(x, y) | x ← [0..c-1], y ← [0..r-1]]


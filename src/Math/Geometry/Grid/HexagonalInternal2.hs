------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.HexGridInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @HexGrid2@ internals. Most developers
-- should use @HexGrid2@ instead. This module is subject to change
-- without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Math.Geometry.Grid.HexagonalInternal2 where

import           GHC.Generics               (Generic)
import           Math.Geometry.GridInternal
import           Prelude                    hiding (null)

data HexDirection = Northwest | North | Northeast | Southeast | South |
                      Southwest deriving (Show, Read, Eq, Generic)

-- | An unbounded grid with hexagonal tiles
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data UnboundedHexGrid = UnboundedHexGrid deriving (Show, Read, Eq, Generic)

instance Grid UnboundedHexGrid where
  type Index UnboundedHexGrid = (Int, Int)
  type Direction UnboundedHexGrid = HexDirection
  indices _ = undefined
  neighbours _ (x,y) =
    [(x-1,y), (x-1,y+1), (x,y+1), (x+1,y), (x+1,y-1), (x,y-1)]
  distance _ (x1, y1) (x2, y2) =
    maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
    where z1 = -x1 - y1
          z2 = -x2 - y2
  directionTo _ (x1, y1) (x2, y2) = f1 . f2 . f3 . f4 . f5 . f6 $ []
    where f1 ds =  if dy > 0 && dz < 0 then North:ds else ds
          f2 ds =  if dy < 0 && dz > 0 then South:ds else ds
          f3 ds =  if dx > 0 && dz < 0 then Northeast:ds else ds
          f4 ds =  if dx < 0 && dy > 0 then Northwest:ds else ds
          f5 ds =  if dx > 0 && dy < 0 then Southeast:ds else ds
          f6 ds =  if dx < 0 && dz > 0 then Southwest:ds else ds
          dx = x2 - x1
          dy = y2 - y1
          z1 = -x1 - y1
          z2 = -x2 - y2
          dz = z2 - z1
  contains _ _ = True
  null _ = False
  nonNull _ = True

--
-- Hexagonal grids with hexagonal tiles
--

-- | A hexagonal grid with hexagonal tiles
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data HexHexGrid = HexHexGrid Int deriving (Show, Read, Eq, Generic)

instance Grid HexHexGrid where
  type Index HexHexGrid = (Int, Int)
  type Direction HexHexGrid = HexDirection
  indices (HexHexGrid r) = [(x, y) | x <- [-r+1..r-1], y <- f x]
    where f x = if x < 0 then [1-r-x .. r-1] else [1-r .. r-1-x]
  neighbours = neighboursBasedOn UnboundedHexGrid
  distance = distanceBasedOn UnboundedHexGrid
  directionTo = directionToBasedOn UnboundedHexGrid
  contains g (x,y) = -s < x && x < s && check
    where s = size g
          check = if x < 0
                    then -s-x < y && y < s
                    else -s < y && y < s-x

instance FiniteGrid HexHexGrid where
  type Size HexHexGrid = Int
  size (HexHexGrid s) = s
  maxPossibleDistance g@(HexHexGrid s) = distance g (-s+1,0) (s-1,0)

instance BoundedGrid HexHexGrid where
  tileSideCount _ = 6
  boundary g =
    northwest ++ northeast ++ east ++ southeast ++ southwest ++ west
    where s = size g
          northwest = [(k,s-1) | k <- [-s+1,-s+2..0]]
          northeast = [(k,s-1-k) | k <- [1,2..s-1]]
          east = [(s-1,k) | k <- [-1,-2..(-s)+1]]
          southeast = [(k,(-s)+1) | k <- [s-2,s-3..0]]
          southwest = [(k,(-s)+1-k) | k <- [-1,-2..(-s)+1]]
          west = [(-s+1,k) | k <- [1,2..s-2]]
  centre _ = [(0,0)]

{-# DEPRECATED hexHexGrid "Use the HexHexGrid constructor instead." #-}

-- | @'hexHexGrid' s@ returns a grid of hexagonal shape, with
--   sides of length @s@, using hexagonal tiles. If @s@ is nonnegative, the
--   resulting grid will have @3*s*(s-1) + 1@ tiles. Otherwise, the resulting
--   grid will be null and the list of indices will be null.
hexHexGrid :: Int -> HexHexGrid
hexHexGrid = HexHexGrid

--
-- Rectangular grids with hexagonal tiles
--

-- | A rectangular grid with hexagonal tiles
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data RectHexGrid = RectHexGrid (Int, Int)
  deriving (Show, Read, Eq, Generic)

instance Grid RectHexGrid where
  type Index RectHexGrid = (Int, Int)
  type Direction RectHexGrid = HexDirection
  indices (RectHexGrid (r, c)) = [(x,rectHexGridY x j) | x <- [0..c-1], j <- [0..r-1]]
  neighbours = neighboursBasedOn UnboundedHexGrid
  distance = distanceBasedOn UnboundedHexGrid
  directionTo = directionToBasedOn UnboundedHexGrid
  contains g (x,y) = 0 <= x && x < c && y0 <= y && y <= y1
    where (r,c) = size g
          y0 = rectHexGridY x 0
          y1 = rectHexGridY x (r-1)
--          (y0,y1) = rectHexGridYEndpoints r x

instance FiniteGrid RectHexGrid where
  type Size RectHexGrid = (Int, Int)
  size (RectHexGrid s) = s
  maxPossibleDistance g@(RectHexGrid (r,c)) =
    distance g (0,0) (c-1,r-(c `div` 2))

instance BoundedGrid RectHexGrid where
  tileSideCount _ = 6
  boundary g =
    [(0,rectHexGridY 0 j) | c>0, j <- [0..r-1]]                -- West
      ++ [(x,rectHexGridY x (r-1)) | r>0, x <- [1..c-1]]       -- North
      ++ [(c-1,rectHexGridY (c-1) j) | c>1, j <- [r-2,r-3..0]] -- East
      ++ [(x,rectHexGridY x 0) | r>1, x <- [c-2,c-3..1]]       -- South
    where (r,c) = size g


{-# DEPRECATED rectHexGrid "Use the RectHexGrid constructor instead." #-}

-- | @'rectHexGrid' r c@ returns a grid in the shape of a
--   parallelogram with @r@ rows and @c@ columns, using hexagonal tiles.
--   If @r@ and @c@ are both nonnegative, the resulting grid will have
--   @r*c@ tiles. Otherwise, the resulting grid will be null and the
--   list of indices will be null.
rectHexGrid :: Int -> Int -> RectHexGrid
rectHexGrid r c = RectHexGrid (r,c)

rectHexGridY :: Int -> Int -> Int
rectHexGridY x j = j - x `div` 2

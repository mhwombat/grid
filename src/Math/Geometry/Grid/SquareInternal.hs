------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.SquareGridInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SquareGrid@ internals. Most developers
-- should use @SquareGrid@ instead. This module is subject to change
-- without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Math.Geometry.Grid.SquareInternal where

import           Prelude                    hiding (null)

import           Data.List                  (nub)
import           GHC.Generics               (Generic)
import           Math.Geometry.GridInternal

data SquareDirection = North | East | South | West
  deriving (Read, Show, Eq)

-- | An unbounded grid with square tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data UnboundedSquareGrid = UnboundedSquareGrid
  deriving (Read, Show, Eq, Generic)

instance Grid UnboundedSquareGrid where
  type Index UnboundedSquareGrid = (Int, Int)
  type Direction UnboundedSquareGrid = SquareDirection
  indices _ = undefined
  neighbours _ (x,y) = [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]
  distance _ (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)
  contains _ _ = True
  directionTo _ (x1, y1) (x2, y2) = f1 . f2 . f3 . f4 $ []
    where f1 ds =  if y2 > y1 then North:ds else ds
          f2 ds =  if y2 < y1 then South:ds else ds
          f3 ds =  if x2 > x1 then East:ds else ds
          f4 ds =  if x2 < x1 then West:ds else ds
  null _ = False
  nonNull _ = True

--
-- Rectangular grids with square tiles
--

-- | A rectangular grid with square tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data RectSquareGrid = RectSquareGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid RectSquareGrid where
  type Index RectSquareGrid = (Int, Int)
  type Direction RectSquareGrid = SquareDirection
  indices (RectSquareGrid (r, c)) = [(x,y) | x <- [0..c-1], y <- [0..r-1]]
  neighbours = neighboursBasedOn UnboundedSquareGrid
  distance = distanceBasedOn UnboundedSquareGrid
  adjacentTilesToward g a@(x1, y1) (x2, y2) =
    filter (\i -> g `contains` i && i /= a) $ nub [(x1,y1+dy),(x1+dx,y1)]
      where dx = signum (x2-x1)
            dy = signum (y2-y1)
  directionTo g x y = if g `contains` x && g `contains` y
                        then directionTo UnboundedSquareGrid x y
                        else []
  contains g (x,y) = 0 <= x && x < c && 0 <= y && y < r
    where (r, c) = size g

instance FiniteGrid RectSquareGrid where
  type Size RectSquareGrid = (Int, Int)
  size (RectSquareGrid s) = s
  maxPossibleDistance g@(RectSquareGrid (r,c)) =
    distance g (0,0) (c-1,r-1)

instance BoundedGrid RectSquareGrid where
  tileSideCount _ = 4
  boundary g = cartesianIndices . size $ g
  centre g = cartesianCentre . size $ g

{-# DEPRECATED rectSquareGrid "Use the RectSquareGrid constructor instead." #-}

-- | @'rectSquareGrid' r c@ produces a rectangular grid with @r@ rows
--   and @c@ columns, using square tiles. If @r@ and @c@ are both
--   nonnegative, the resulting grid will have @r*c@ tiles. Otherwise,
--   the resulting grid will be null and the list of indices will be
--   null.
rectSquareGrid :: Int -> Int -> RectSquareGrid
rectSquareGrid r c = RectSquareGrid (r,c)

--
-- Toroidal grids with square tiles.
--

-- | A toroidal grid with square tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TorSquareGrid = TorSquareGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid TorSquareGrid where
  type Index TorSquareGrid = (Int, Int)
  type Direction TorSquareGrid = SquareDirection
  indices (TorSquareGrid (r, c)) = [(x, y) | x <- [0..c-1], y <- [0..r-1]]
  neighbours = neighboursWrappedBasedOn UnboundedSquareGrid
  neighbour = neighbourWrappedBasedOn UnboundedSquareGrid
  distance = distanceWrappedBasedOn UnboundedSquareGrid
  directionTo = directionToWrappedBasedOn UnboundedSquareGrid
  isAdjacent g a b = distance g a b <= 1
  contains _ _ = True

instance FiniteGrid TorSquareGrid where
  type Size TorSquareGrid = (Int, Int)
  size (TorSquareGrid s) = s
  maxPossibleDistance g@(TorSquareGrid (r,c)) =
    distance g (0,0) (c `div` 2, r `div` 2)

instance WrappedGrid TorSquareGrid where
  normalise g (x,y) = (x `mod` c, y `mod` r)
    where (r, c) = size g
  denormalise g b = nub [ (x-c,y+r), (x,y+r), (x+c,y+r),
                          (x-c,y),   (x,y),   (x+c,y),
                          (x-c,y-r), (x,y-r), (x+c,y-r) ]
    where (r, c) = size g
          (x, y) = normalise g b

{-# DEPRECATED torSquareGrid "Use the TorSquareGrid constructor instead." #-}

-- | @'torSquareGrid' r c@ returns a toroidal grid with @r@
--   rows and @c@ columns, using square tiles. If @r@ and @c@ are
--   both nonnegative, the resulting grid will have @r*c@ tiles. Otherwise,
--   the resulting grid will be null and the list of indices will be null.
torSquareGrid :: Int -> Int -> TorSquareGrid
torSquareGrid r c = TorSquareGrid (r,c)


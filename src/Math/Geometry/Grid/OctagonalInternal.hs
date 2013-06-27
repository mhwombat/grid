------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.OctGridInternal
-- Copyright   :  (c) Amy de Buitléir 2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @OctGrid@ internals. Most developers 
-- should use @OctGrid@ instead. This module is subject to change 
-- without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Math.Geometry.Grid.OctagonalInternal where

import Prelude hiding (null)

import Data.List (nub)
import Math.Geometry.GridInternal

data OctDirection = West | Northwest | North | Northeast | East | 
                      Southeast | South | Southwest deriving (Show, Eq)

-- | An unbounded grid with octagonal tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data UnboundedOctGrid = UnboundedOctGrid deriving Show

instance Grid UnboundedOctGrid where
  type Index UnboundedOctGrid = (Int, Int)
  type Direction UnboundedOctGrid = OctDirection
  indices _ = undefined
  neighbours _ (x,y) = [(x-1,y+1), (x,y+1), (x+1,y+1), (x+1,y), 
                        (x+1,y-1), (x,y-1), (x-1,y-1), (x-1,y)]
  distance _ (x1, y1) (x2, y2) = max (abs (x2-x1)) (abs (y2-y1))
  contains _ _ = True
  directionTo _ (x1, y1) (x2, y2) = 
    f1 . f2 . f3 . f4 . f5 . f6 . f7 . f8 $ []
    where f1 ds =  if  dy > abs dx then North:ds else ds
          f2 ds =  if -dy > abs dx then South:ds else ds
          f3 ds =  if  dx > abs dy then East:ds else ds
          f4 ds =  if -dx > abs dy then West:ds else ds
          f5 ds =  if dx > 0 && dy > 0 then Northeast:ds else ds
          f6 ds =  if dx > 0 && dy < 0 then Southeast:ds else ds
          f7 ds =  if dx < 0 && dy < 0 then Southwest:ds else ds
          f8 ds =  if dx < 0 && dy > 0 then Northwest:ds else ds
          dx = x2 - x1
          dy = y2 - y1
  null _ = False
  nonNull _ = True

--
-- Rectangular grids with octagonal tiles
--

-- | A rectangular grid with octagonal tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data RectOctGrid = RectOctGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show RectOctGrid where 
  show (RectOctGrid (r,c) _) = 
    "rectOctGrid " ++ show r ++ " " ++ show c

instance Grid RectOctGrid where
  type Index RectOctGrid = (Int, Int)
  type Direction RectOctGrid = OctDirection
  indices (RectOctGrid _ xs) = xs
  neighbours = neighboursBasedOn UnboundedOctGrid
  distance = distanceBasedOn UnboundedOctGrid
  directionTo = directionToBasedOn UnboundedOctGrid
  contains g (x,y) = 0 <= x && x < c && 0 <= y && y < r
    where (r,c) = size g

instance FiniteGrid RectOctGrid where
  type Size RectOctGrid = (Int, Int)
  size (RectOctGrid s _) = s
  maxPossibleDistance g@(RectOctGrid (r,c) _) = 
    distance g (0,0) (c-1,r-1)

instance BoundedGrid RectOctGrid where
  tileSideCount _ = 8
  boundary g = cartesianIndices . size $ g
  centre g = cartesianCentre . size $ g

-- | @'rectOctGrid' r c@ produces a rectangular grid with @r@ rows
--   and @c@ columns, using octagonal tiles. If @r@ and @c@ are both 
--   nonnegative, the resulting grid will have @r*c@ tiles. Otherwise, 
--   the resulting grid will be null and the list of indices will be 
--   null.
rectOctGrid :: Int -> Int -> RectOctGrid
rectOctGrid r c = 
  RectOctGrid (r,c) [(x,y) | x <- [0..c-1], y <- [0..r-1]]

--
-- Toroidal grids with octagonal tiles.
--

-- | A toroidal grid with octagonal tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TorOctGrid = TorOctGrid (Int, Int) [(Int, Int)] deriving Eq

instance Show TorOctGrid where 
  show (TorOctGrid (r,c) _) = "torOctGrid " ++ show r ++ " " ++ show c

instance Grid TorOctGrid where
  type Index TorOctGrid = (Int, Int)
  type Direction TorOctGrid = OctDirection
  indices (TorOctGrid _ xs) = xs
  neighbours = neighboursWrappedBasedOn UnboundedOctGrid
  neighbour = neighbourWrappedBasedOn UnboundedOctGrid
  distance = distanceWrappedBasedOn UnboundedOctGrid
  directionTo = directionToWrappedBasedOn UnboundedOctGrid
  isAdjacent g a b = distance g a b <= 1
  contains _ _ = True

instance FiniteGrid TorOctGrid where
  type Size TorOctGrid = (Int, Int)
  size (TorOctGrid s _) = s
  maxPossibleDistance g@(TorOctGrid (r,c) _) =
    distance g (0,0) (c `div` 2, r `div` 2)

instance WrappedGrid TorOctGrid where
  normalise g (x,y) = (x `mod` c, y `mod` r)
    where (r, c) = size g
  denormalise g a = nub [ (x-c,y+r), (x,y+r), (x+c,y+r), 
                          (x-c,y),   (x,y),   (x+c,y),
                          (x-c,y-r), (x,y-r), (x+c,y-r) ]
    where (r, c) = size g
          (x, y) = normalise g a

-- | @'torOctGrid' r c@ returns a toroidal grid with @r@ 
--   rows and @c@ columns, using octagonal tiles. If @r@ and @c@ are 
--   both nonnegative, the resulting grid will have @r*c@ tiles. Otherwise, 
--   the resulting grid will be null and the list of indices will be null.
torOctGrid :: Int -> Int -> TorOctGrid
torOctGrid r c = TorOctGrid (r,c) [(x, y) | x <- [0..c-1], y <- [0..r-1]]


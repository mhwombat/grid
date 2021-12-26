------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid.SquareQC
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.Grid.SquareQC
  (
    test
  ) where

import           Math.Geometry.Grid.SquareInternal
import           Math.Geometry.GridInternal
import           Math.Geometry.GridQC

import           Prelude                           hiding (null)
import           Test.Framework                    (Test, testGroup)
import           Test.QuickCheck                   (Arbitrary, Gen, Property,
                                                    arbitrary, choose, elements,
                                                    sized, vectorOf)

instance Arbitrary SquareDirection where
  arbitrary =
    elements [North, South, East, West]

--
-- Unbounded grids with square tiles
--

data UnboundedSquareGridTD =
  UnboundedSquareGridTD [(Int,Int)] ((Int,Int),(Int,Int)) SquareDirection
  deriving (Show, Read)

instance TestData UnboundedSquareGridTD where
  type BaseGrid UnboundedSquareGridTD = UnboundedSquareGrid
  grid _ = UnboundedSquareGrid
  points (UnboundedSquareGridTD ps _ _) = ps
  twoClosePoints (UnboundedSquareGridTD _ qs _) = qs
  neighbourCountBounds _ = (4, 4)
  direction (UnboundedSquareGridTD _ _ d) = d

sizedUnboundedSquareGridTD :: Int -> Gen UnboundedSquareGridTD
sizedUnboundedSquareGridTD n = do
  k <- choose (0,n)
  ps <- vectorOf (k+2) arbitrary :: Gen [(Int,Int)]
  qs <- chooseClosePointsUnbounded
  UnboundedSquareGridTD ps qs <$> arbitrary

instance Arbitrary UnboundedSquareGridTD where
  arbitrary = sized sizedUnboundedSquareGridTD

unboundedSquareGridProperties :: [(String, UnboundedSquareGridTD -> Property)]
unboundedSquareGridProperties = gridProperties "UnboundedSquareGrid"

unboundedSquareGridTests :: [Test]
unboundedSquareGridTests = makeTests unboundedSquareGridProperties


--
-- Rectangular grids with square tiles
--

data RectSquareGridTD =
  RectSquareGridTD RectSquareGrid [(Int,Int)] ((Int,Int),(Int,Int)) SquareDirection
  deriving (Show, Read)

instance TestData RectSquareGridTD where
  type BaseGrid RectSquareGridTD = RectSquareGrid
  grid (RectSquareGridTD g _ _ _) = g
  points (RectSquareGridTD _ ps _ _) = ps
  twoClosePoints (RectSquareGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 4)
  direction (RectSquareGridTD _ _ _ d) = d

instance TestDataF RectSquareGridTD where
  maxDistance (RectSquareGridTD g _ _ _) = r + c - 2
    where (r, c) = size g
  expectedTileCount (RectSquareGridTD g _ _ _) = r*c
    where (r,c) = size g

instance TestDataB RectSquareGridTD where
  expectedBoundaryCount (RectSquareGridTD g _ _ _) =
    (cartesianBoundaryCount . size) g

-- We want the number of tiles in a test grid to be O(n)
sizedRectSquareGridTD :: Int -> Gen RectSquareGridTD
sizedRectSquareGridTD n = do
  r <- choose (0,n)
  let c = n `div` (r+1)
  let g = RectSquareGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  RectSquareGridTD g ps qs <$> arbitrary

instance Arbitrary RectSquareGridTD where
  arbitrary = sized sizedRectSquareGridTD

rectSquareGridProperties :: [(String, RectSquareGridTD -> Property)]
rectSquareGridProperties = gridProperties "RectSquareGrid"
  ++ finiteGridProperties "RectSquareGrid"
  ++ boundedGridProperties "RectSquareGrid"
  ++ boundedGridProperties2 "RectSquareGrid"

rectSquareGridTests :: [Test]
rectSquareGridTests = makeTests rectSquareGridProperties


--
-- Toroidal grids with square tiles
--

data TorSquareGridTD =
  TorSquareGridTD TorSquareGrid [(Int,Int)] ((Int,Int),(Int,Int)) SquareDirection
  deriving (Show, Read)

instance TestData TorSquareGridTD where
  type BaseGrid TorSquareGridTD = TorSquareGrid
  grid (TorSquareGridTD g _ _ _) = g
  points (TorSquareGridTD _ ps _ _) = ps
  twoClosePoints (TorSquareGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 4)
  direction (TorSquareGridTD _ _ _ d) = d

instance TestDataF TorSquareGridTD where
  maxDistance (TorSquareGridTD g _ _ _) = (r+c) `div` 2
    where (r, c) = size g
  expectedTileCount (TorSquareGridTD g _ _ _) = r*c
    where (r,c) = size g

-- We want the number of tiles in a test grid to be O(n)
sizedTorSquareGridTD :: Int -> Gen TorSquareGridTD
sizedTorSquareGridTD n = do
  r <- choose (0,n)
  let c = n `div` (r+1)
  let g = TorSquareGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  TorSquareGridTD g ps qs <$> arbitrary

instance Arbitrary TorSquareGridTD where
  arbitrary = sized sizedTorSquareGridTD

torSquareGridProperties :: [(String, TorSquareGridTD -> Property)]
torSquareGridProperties = gridProperties "TorSquareGrid"
  ++ finiteGridProperties "TorSquareGrid"

torSquareGridTests :: [Test]
torSquareGridTests = makeTests torSquareGridProperties

--TODO replace these
--TODO replace these
--TODO replace these

--prop_UnboundedSquareGrid_num_min_paths_correct ::
--  UnboundedSquareGrid -> Int -> Int -> Property
--prop_UnboundedSquareGrid_num_min_paths_correct g i j = nonNull g ==>
--  minPathCount g a b == M.choose (deltaX+deltaY) deltaX
--    where a = g `pointAt` i
--          b = g `pointAt` j
--          deltaX = abs $ fst b - fst a
--          deltaY = abs $ snd b - snd a

---- If the ordering produced by rectSquareGrid is ever changed, this
---- property may need to be changed too. It relies on the first and last
---- elements being at opposite corners.
--prop_RectSquareGrid_distance_corner_to_corner :: RectSquareGrid -> Property
--prop_RectSquareGrid_distance_corner_to_corner g = r > 0 && c > 0 ==>
--  distance g a b == r + c - 2
--    where (r, c) = size g
--          ps = indices g
--          a = head ps
--          b = last ps

--prop_RectSquareGrid_num_min_paths_correct ::
--  RectSquareGrid -> Int -> Int -> Property
--prop_RectSquareGrid_num_min_paths_correct g i j = nonNull g ==>
--  minPathCount g a b == M.choose (deltaX+deltaY) deltaX
--    where a = g `pointAt` i
--          b = g `pointAt` j
--          deltaX = abs $ fst b - fst a
--          deltaY = abs $ snd b - snd a

---- If the ordering produced by torSquareGrid is ever changed, this property
---- may need to be changed too.
--prop_TorSquareGrid_distance_corner_to_corner :: TorSquareGrid -> Property
--prop_TorSquareGrid_distance_corner_to_corner g = r > 0 && c > 0 ==>
--  distance g a b == f
--    where (r, c) = size g
--          ps = indices g
--          a = head ps
--          b = last ps
--          f | r == 1 && c == 1 = 0 -- single-tile torus
--            | r == 1 || c == 1 = 1 -- a and b are the same
--            | otherwise      = 2


test :: Test
test = testGroup "Math.Geometry.Grid.SquareQC"
  (unboundedSquareGridTests ++ rectSquareGridTests ++ torSquareGridTests)



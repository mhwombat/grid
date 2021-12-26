------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid.OctagonalQC
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

module Math.Geometry.Grid.OctagonalQC
  (
    test
  ) where

import           Math.Geometry.Grid.OctagonalInternal
import           Math.Geometry.GridInternal
import           Math.Geometry.GridQC

import           Prelude                              hiding (null)
import           Test.Framework                       (Test, testGroup)
import           Test.QuickCheck                      (Arbitrary, Gen, Property,
                                                       arbitrary, choose,
                                                       elements, sized,
                                                       vectorOf)

instance Arbitrary OctDirection where
  arbitrary = elements [West, Northwest, North, Northeast, East,
                        Southeast, South, Southwest]

--
-- Unbounded grids with octagonal tiles
--

data UnboundedOctGridTD =
  UnboundedOctGridTD [(Int,Int)] ((Int,Int),(Int,Int)) OctDirection
  deriving (Show, Read)

instance TestData UnboundedOctGridTD where
  type BaseGrid UnboundedOctGridTD = UnboundedOctGrid
  grid _ = UnboundedOctGrid
  points (UnboundedOctGridTD ps _ _) = ps
  twoClosePoints (UnboundedOctGridTD _ qs _) = qs
  neighbourCountBounds _ = (8, 8)
  direction (UnboundedOctGridTD _ _ d) = d

sizedUnboundedOctGridTD :: Int -> Gen UnboundedOctGridTD
sizedUnboundedOctGridTD n = do
  k <- choose (0,n)
  ps <- vectorOf (k+2) arbitrary :: Gen [(Int,Int)]
  qs <- chooseClosePointsUnbounded
  UnboundedOctGridTD ps qs <$> arbitrary

instance Arbitrary UnboundedOctGridTD where
  arbitrary = sized sizedUnboundedOctGridTD

unboundedOctGridProperties :: [(String, UnboundedOctGridTD -> Property)]
unboundedOctGridProperties = gridProperties "UnboundedOctGrid"

unboundedOctGridTests :: [Test]
unboundedOctGridTests = makeTests unboundedOctGridProperties

--
-- Rectangular grids with octagonal tiles
--

data RectOctGridTD =
  RectOctGridTD RectOctGrid [(Int,Int)] ((Int,Int),(Int,Int)) OctDirection
  deriving (Show, Read)

instance TestData RectOctGridTD where
  type BaseGrid RectOctGridTD = RectOctGrid
  grid (RectOctGridTD g _ _ _) = g
  points (RectOctGridTD _ ps _ _) = ps
  twoClosePoints (RectOctGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 8)
  direction (RectOctGridTD _ _ _ d) = d

instance TestDataF RectOctGridTD where
  maxDistance (RectOctGridTD g _ _ _) = max r c - 1
    where (r, c) = size g
  expectedTileCount (RectOctGridTD g _ _ _) = r*c
    where (r,c) = size g

instance TestDataB RectOctGridTD where
  expectedBoundaryCount (RectOctGridTD g _ _ _) =
    (cartesianBoundaryCount . size) g

-- We want the number of tiles in a test grid to be O(n)
sizedRectOctGridTD :: Int -> Gen RectOctGridTD
sizedRectOctGridTD n = do
--  let n' = min n 12 -- calculation time for these grids grows quickly!
--  r <- choose (0,n')
--  let c = n' `div` (r+1)
  r <- choose (0,n)
  let c = n `div` (r+1)
  let g = RectOctGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  RectOctGridTD g ps qs <$> arbitrary

instance Arbitrary RectOctGridTD where
  arbitrary = sized sizedRectOctGridTD

rectOctGridProperties :: [(String, RectOctGridTD -> Property)]
rectOctGridProperties = gridProperties "RectOctGrid"
  ++ finiteGridProperties "RectOctGrid"
  ++ boundedGridProperties "RectOctGrid"
  ++ boundedGridProperties2 "RectOctGrid"

rectOctGridTests :: [Test]
rectOctGridTests = makeTests rectOctGridProperties


--
-- Toroidal grids with octagonal tiles
--

data TorOctGridTD =
  TorOctGridTD TorOctGrid [(Int,Int)] ((Int,Int),(Int,Int)) OctDirection
  deriving (Show, Read)

instance TestData TorOctGridTD where
  type BaseGrid TorOctGridTD = TorOctGrid
  grid (TorOctGridTD g _ _ _) = g
  points (TorOctGridTD _ ps _ _) = ps
  twoClosePoints (TorOctGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 8)
  direction (TorOctGridTD _ _ _ d) = d

instance TestDataF TorOctGridTD where
  maxDistance (TorOctGridTD g _ _ _) = min r c + abs (r-c)
    where (r, c) = size g
  expectedTileCount (TorOctGridTD g _ _ _) = r*c
    where (r,c) = size g

-- We want the number of tiles in a test grid to be O(n)
sizedTorOctGridTD :: Int -> Gen TorOctGridTD
sizedTorOctGridTD n = do
  r <- choose (0,n)
  let c = n `div` (r+1)
  let g = TorOctGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  TorOctGridTD g ps qs <$> arbitrary

instance Arbitrary TorOctGridTD where
  arbitrary = sized sizedTorOctGridTD

torOctGridProperties :: [(String, TorOctGridTD -> Property)]
torOctGridProperties = gridProperties "TorOctGrid"
  ++ finiteGridProperties "TorOctGrid"

torOctGridTests :: [Test]
torOctGridTests = makeTests torOctGridProperties



--TODO redo these

--prop_RectOctGrid_num_min_paths_correct ::
--  RectOctGrid -> Int -> Int -> Property
--prop_RectOctGrid_num_min_paths_correct g i j = nonNull g ==>
--  minPathCount g a b ==
--    if a == b then 1 else minPathCount2 g att b
--    where a = g `pointAt` i
--          b = g `pointAt` j
--          att = adjacentTilesToward g a b


test :: Test
test = testGroup "Math.Geometry.Grid.OctagonalQC"
  (unboundedOctGridTests ++ rectOctGridTests ++ torOctGridTests)



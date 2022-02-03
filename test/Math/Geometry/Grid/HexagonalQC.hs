------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid.RectangularQC
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
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

module Math.Geometry.Grid.HexagonalQC
  (
    test
  ) where

import           Math.Geometry.Grid.HexagonalInternal
import           Math.Geometry.GridInternal
import           Math.Geometry.GridQC

import           Prelude                              hiding (null)
import           Test.Framework                       (Test, testGroup)
import           Test.QuickCheck                      (Arbitrary, Gen, Property,
                                                       arbitrary, choose,
                                                       elements, sized,
                                                       vectorOf)

instance Arbitrary HexDirection where
  arbitrary =
    elements [West, Northwest, Northeast, East, Southeast, Southwest]

--
-- Unbounded grids with hexagonal tiles
--

data UnboundedHexGridTD =
  UnboundedHexGridTD [(Int,Int)] ((Int,Int),(Int,Int)) HexDirection
  deriving (Show, Read)

instance TestData UnboundedHexGridTD where
  type BaseGrid UnboundedHexGridTD = UnboundedHexGrid
  grid _ = UnboundedHexGrid
  points (UnboundedHexGridTD ps _ _) = ps
  twoClosePoints (UnboundedHexGridTD _ qs _) = qs
  neighbourCountBounds _ = (6, 6)
  direction (UnboundedHexGridTD _ _ d) = d

sizedUnboundedHexGridTD :: Int -> Gen UnboundedHexGridTD
sizedUnboundedHexGridTD n = do
  k <- choose (0,n)
  ps <- vectorOf (k+2) arbitrary :: Gen [(Int,Int)]
  qs <- chooseClosePointsUnbounded
  UnboundedHexGridTD ps qs <$> arbitrary

instance Arbitrary UnboundedHexGridTD where
  arbitrary = sized sizedUnboundedHexGridTD

unboundedHexGridProperties :: [(String, UnboundedHexGridTD -> Property)]
unboundedHexGridProperties = gridProperties "UnboundedHexGrid"

unboundedHexGridTests :: [Test]
unboundedHexGridTests = makeTests unboundedHexGridProperties

--
-- Hegagonal grids with hexagonal tiles
--

data HexHexGridTD =
  HexHexGridTD HexHexGrid [(Int,Int)] ((Int,Int),(Int,Int)) HexDirection
  deriving (Show, Read)

instance TestData HexHexGridTD where
  type BaseGrid HexHexGridTD = HexHexGrid
  grid (HexHexGridTD g _ _ _) = g
  points (HexHexGridTD _ ps _ _) = ps
  twoClosePoints (HexHexGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 6)
  direction (HexHexGridTD _ _ _ d) = d

instance TestDataF HexHexGridTD where
  maxDistance (HexHexGridTD g _ _ _) = 2*s - 2
    where s = size g
  expectedTileCount (HexHexGridTD g _ _ _) = 3*s*(s-1) + 1
    where s = size g

instance TestDataB HexHexGridTD where
  expectedBoundaryCount (HexHexGridTD g _ _ _) = (f . size) g
    where f 0 = 0
          f 1 = 1
          f s = 6*(s-1)

-- We want the number of tiles in a test grid to be O(n)
sizedHexHexGridTD :: Int -> Gen HexHexGridTD
sizedHexHexGridTD n = do
  let s = isqrt (n `div` 3)
  let g = HexHexGrid s
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  HexHexGridTD g ps qs <$> arbitrary

instance Arbitrary HexHexGridTD where
  arbitrary = sized sizedHexHexGridTD

hexHexGridProperties :: [(String, HexHexGridTD -> Property)]
hexHexGridProperties = gridProperties "HexHexGrid"
  ++ finiteGridProperties "HexHexGrid"
  ++ boundedGridProperties "HexHexGrid"
  ++ boundedGridProperties2 "HexHexGrid"

hexHexGridTests :: [Test]
hexHexGridTests = makeTests hexHexGridProperties

--
-- Parallelogrammatical hexagonal grids
--

data ParaHexGridTD =
  ParaHexGridTD ParaHexGrid [(Int,Int)] ((Int,Int),(Int,Int)) HexDirection
  deriving (Show, Read)

instance TestData ParaHexGridTD where
  type BaseGrid ParaHexGridTD = ParaHexGrid
  grid (ParaHexGridTD g _ _ _) = g
  points (ParaHexGridTD _ ps _ _) = ps
  twoClosePoints (ParaHexGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 6)
  direction (ParaHexGridTD _ _ _ d) = d

instance TestDataF ParaHexGridTD where
  maxDistance (ParaHexGridTD g _ _ _) = r+c-2
    where (r, c) = size g
  expectedTileCount (ParaHexGridTD g _ _ _) = r*c
    where (r,c) = size g

instance TestDataB ParaHexGridTD where
  expectedBoundaryCount (ParaHexGridTD g _ _ _) =
    (cartesianBoundaryCount . size) g

-- We want the number of tiles in a test grid to be O(n)
sizedParaHexGridTD :: Int -> Gen ParaHexGridTD
sizedParaHexGridTD n = do
  r <- choose (0,n)
  let c = n `div` (r+1)
  let g = ParaHexGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  ParaHexGridTD g ps qs <$> arbitrary

instance Arbitrary ParaHexGridTD where
  arbitrary = sized sizedParaHexGridTD

paraHexGridProperties :: [(String, ParaHexGridTD -> Property)]
paraHexGridProperties = gridProperties "ParaHexGrid"
  ++ finiteGridProperties "ParaHexGrid"
  ++ boundedGridProperties "ParaHexGrid"
  ++ boundedGridProperties2 "ParaHexGrid"

paraHexGridTests :: [Test]
paraHexGridTests = makeTests paraHexGridProperties

test :: Test
test = testGroup "Math.Geometry.Grid.HexagonalQC"
  (unboundedHexGridTests ++ hexHexGridTests ++ paraHexGridTests)



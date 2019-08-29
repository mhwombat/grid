------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid.Hexagonal2QC
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ExistentialQuantification,
    TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.Grid.Hexagonal2QC
  (
    test
  ) where

import Math.Geometry.Grid.HexagonalInternal2
import Math.Geometry.GridInternal 
import Math.Geometry.GridQC

import Prelude hiding (null)
import Test.Framework (Test, testGroup)
import Test.QuickCheck 
  (Gen, Arbitrary, arbitrary, sized, elements, choose, Property, vectorOf)

instance Arbitrary HexDirection where
  arbitrary =
    elements [Northwest, North, Northeast, Southeast, South, Southwest]

--
-- Unbounded grids with hexagonal tiles
--

data UnboundedHexGridTD = 
  UnboundedHexGridTD [(Int,Int)] ((Int,Int),(Int,Int)) HexDirection
  deriving Show

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
  d <- arbitrary
  return $ UnboundedHexGridTD ps qs d

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
  deriving Show

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
  let g = hexHexGrid s
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  d <- arbitrary
  return $ HexHexGridTD g ps qs d

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
-- Rectangular hexagonal grids   
--

data RectHexGridTD = 
  RectHexGridTD RectHexGrid [(Int,Int)] ((Int,Int),(Int,Int)) HexDirection
  deriving Show

instance TestData RectHexGridTD where
  type BaseGrid RectHexGridTD = RectHexGrid
  grid (RectHexGridTD g _ _ _) = g
  points (RectHexGridTD _ ps _ _) = ps 
  twoClosePoints (RectHexGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 6)
  direction (RectHexGridTD _ _ _ d) = d

instance TestDataF RectHexGridTD where
  maxDistance (RectHexGridTD g _ _ _) = r+c-2
    where (r, c) = size g
  expectedTileCount (RectHexGridTD g _ _ _) = r*c
    where (r,c) = size g

instance TestDataB RectHexGridTD where
  expectedBoundaryCount (RectHexGridTD g _ _ _) = 
    (cartesianBoundaryCount . size) g

-- We want the number of tiles in a test grid to be O(n)
sizedRectHexGridTD :: Int -> Gen RectHexGridTD
sizedRectHexGridTD n = do
  r <- choose (0,n)
  let c0 = n `div` (r+1)
  let c = 2*(c0 `div` 2) -- force it to be even
  let g = rectHexGrid r c
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  d <- arbitrary
  return $ RectHexGridTD g ps qs d

instance Arbitrary RectHexGridTD where
  arbitrary = sized sizedRectHexGridTD

rectHexGridProperties :: [(String, RectHexGridTD -> Property)]
rectHexGridProperties = gridProperties "RectHexGrid"
  ++ finiteGridProperties "RectHexGrid"
  ++ boundedGridProperties "RectHexGrid"
  ++ boundedGridProperties2 "RectHexGrid"

rectHexGridTests :: [Test]
rectHexGridTests = makeTests rectHexGridProperties

test :: Test
test = testGroup "Math.Geometry.Grid.Hexagonal2QC"
  (unboundedHexGridTests ++ hexHexGridTests ++ rectHexGridTests)



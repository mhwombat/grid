------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid.TriangularQC
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

module Math.Geometry.Grid.TriangularQC
  (
    test
  ) where

import           Math.Geometry.Grid.TriangularInternal
import           Math.Geometry.GridInternal
import           Math.Geometry.GridQC

import           Prelude                               hiding (null)
import           Test.Framework                        (Test, testGroup)
import           Test.QuickCheck                       (Arbitrary, Gen,
                                                        Property, arbitrary,
                                                        choose, elements, sized,
                                                        suchThat, vectorOf)

instance Arbitrary TriDirection where
  arbitrary =
    elements [South, Northwest, Northeast, North, Southeast, Southwest]

--
-- Unbounded grids with triangular tiles
--

data UnboundedTriGridTD =
  UnboundedTriGridTD [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData UnboundedTriGridTD where
  type BaseGrid UnboundedTriGridTD = UnboundedTriGrid
  grid _ = UnboundedTriGrid
  points (UnboundedTriGridTD ps _ _) = ps
  twoClosePoints (UnboundedTriGridTD _ qs _) = qs
  neighbourCountBounds _ = (3, 3)
  direction (UnboundedTriGridTD _ _ d) = d


valid :: (Int,Int) -> Bool
valid (x,y) = even (x+y)

bothValid :: ((Int,Int),(Int,Int)) -> Bool
bothValid (a,b) = valid a && valid b

sizedUnboundedTriGridTD :: Int -> Gen UnboundedTriGridTD
sizedUnboundedTriGridTD n = do
  k <- choose (0,n)
  ps <- vectorOf (k+2) (arbitrary `suchThat` valid) :: Gen [(Int,Int)]
  qs <- chooseClosePointsUnbounded `suchThat` bothValid
  UnboundedTriGridTD ps qs <$> arbitrary

instance Arbitrary UnboundedTriGridTD where
  arbitrary = sized sizedUnboundedTriGridTD

unboundedTriGridProperties :: [(String, UnboundedTriGridTD -> Property)]
unboundedTriGridProperties = gridProperties "UnboundedTriGrid"

unboundedTriGridTests :: [Test]
unboundedTriGridTests = makeTests unboundedTriGridProperties


--
-- Triangular grids with triangular tiles
--

data TriTriGridTD =
  TriTriGridTD TriTriGrid [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData TriTriGridTD where
  type BaseGrid TriTriGridTD = TriTriGrid
  grid (TriTriGridTD g _ _ _) = g
  points (TriTriGridTD _ ps _ _) = ps
  twoClosePoints (TriTriGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 3)
  direction (TriTriGridTD _ _ _ d) = d

instance TestDataF TriTriGridTD where
  maxDistance (TriTriGridTD g _ _ _) = 2*(s-1)
    where s = size g
  expectedTileCount (TriTriGridTD g _ _ _) = s*s
    where s = size g

instance TestDataB TriTriGridTD where
  expectedBoundaryCount (TriTriGridTD g _ _ _) = (f . size) g
    where f 0 = 0
          f 1 = 1
          f s = 3*(s-1)

-- We want the number of tiles in a test grid to be O(n)
sizedTriTriGridTD :: Int -> Gen TriTriGridTD
sizedTriTriGridTD n = do
  let g = TriTriGrid (2 * isqrt n)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g `suchThat` bothValid
  TriTriGridTD g ps qs <$> arbitrary

instance Arbitrary TriTriGridTD where
  arbitrary = sized sizedTriTriGridTD

triTriGridProperties :: [(String, TriTriGridTD -> Property)]
triTriGridProperties = gridProperties "TriTriGrid"
  ++ finiteGridProperties "TriTriGrid"
  ++ boundedGridProperties "TriTriGrid"

triTriGridTests :: [Test]
triTriGridTests = makeTests triTriGridProperties

--
-- Parallelogram-shaped grids with triangular tiles
--

data ParaTriGridTD =
  ParaTriGridTD ParaTriGrid [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData ParaTriGridTD where
  type BaseGrid ParaTriGridTD = ParaTriGrid
  grid (ParaTriGridTD g _ _ _) = g
  points (ParaTriGridTD _ ps _ _) = ps
  twoClosePoints (ParaTriGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 3)
  direction (ParaTriGridTD _ _ _ d) = d

instance TestDataF ParaTriGridTD where
  maxDistance (ParaTriGridTD g _ _ _) = 2*(r+c) - 3
    where (r, c) = size g
  expectedTileCount (ParaTriGridTD g _ _ _) = 2*r*c
    where (r, c) = size g

instance TestDataB ParaTriGridTD where
  expectedBoundaryCount (ParaTriGridTD g _ _ _) = (f . size) g
    where f (0,_) = 0
          f (_,0) = 0
          f (1,c) = 2*c
          f (r,1) = 2*r
          f (r,c) = 2*(r+c-1)

-- We want the number of tiles in a test grid to be O(n)
sizedParaTriGridTD :: Int -> Gen ParaTriGridTD
sizedParaTriGridTD n = do
  r <- choose (0,n)
  let c = n `div` (2*r + 1)
  let g = ParaTriGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g `suchThat` bothValid
  ParaTriGridTD g ps qs <$> arbitrary

instance Arbitrary ParaTriGridTD where
  arbitrary = sized sizedParaTriGridTD

paraTriGridProperties :: [(String, ParaTriGridTD -> Property)]
paraTriGridProperties = gridProperties "ParaTriGrid"
  ++ finiteGridProperties "ParaTriGrid"
  ++ boundedGridProperties "ParaTriGrid"

paraTriGridTests :: [Test]
paraTriGridTests = makeTests paraTriGridProperties


--
-- Rectangular grids with triangular tiles
--

data RectTriGridTD =
  RectTriGridTD RectTriGrid [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData RectTriGridTD where
  type BaseGrid RectTriGridTD = RectTriGrid
  grid (RectTriGridTD g _ _ _) = g
  points (RectTriGridTD _ ps _ _) = ps
  twoClosePoints (RectTriGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 3)
  direction (RectTriGridTD _ _ _ d) = d

instance TestDataF RectTriGridTD where
  maxDistance (RectTriGridTD g _ _ _) = 2*(r+c) - 3
    where (r, c) = size g
  expectedTileCount (RectTriGridTD g _ _ _) = 2*r*c
    where (r, c) = size g

instance TestDataB RectTriGridTD where
  expectedBoundaryCount (RectTriGridTD g _ _ _) = (f . size) g
    where f (0,_) = 0
          f (_,0) = 0
          f (1,c) = 2*c
          f (r,1) = 2*r
          f (r,c) = 2*(r+c-1)

-- We want the number of tiles in a test grid to be O(n)
sizedRectTriGridTD :: Int -> Gen RectTriGridTD
sizedRectTriGridTD n = do
  r <- choose (0,n)
  let c = n `div` (2*r + 1)
  let g = RectTriGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g `suchThat`
    (\(a,b) -> bothValid (a,b) && inRectBounds r c a && inRectBounds r c b)
  RectTriGridTD g ps qs <$> arbitrary

inRectBounds :: Int -> Int -> (Int, Int) -> Bool
inRectBounds _ c (x, y) = xMin <= x && x <= xMax
  where xMin = if even y then w else w+1
        w = -2*((y+1) `div` 4)
        xMax = xMin + 2*(c-1)

instance Arbitrary RectTriGridTD where
  arbitrary = sized sizedRectTriGridTD

rectTriGridProperties :: [(String, RectTriGridTD -> Property)]
rectTriGridProperties = gridProperties "RectTriGrid"
  ++ finiteGridProperties "RectTriGrid"
  ++ boundedGridProperties "RectTriGrid"

rectTriGridTests :: [Test]
rectTriGridTests = makeTests rectTriGridProperties


--
-- Toroidal grids with triangular tiles
--

data TorTriGridTD =
  TorTriGridTD TorTriGrid [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData TorTriGridTD where
  type BaseGrid TorTriGridTD = TorTriGrid
  grid (TorTriGridTD g _ _ _) = g
  points (TorTriGridTD _ ps _ _) = ps
  twoClosePoints (TorTriGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 3)
  direction (TorTriGridTD _ _ _ d) = d

instance TestDataF TorTriGridTD where
  maxDistance (TorTriGridTD g _ _ _) = 2*(r+c) - 3
    where (r, c) = size g
  expectedTileCount (TorTriGridTD g _ _ _) = 2*r*c
    where (r, c) = size g

-- We want the number of tiles in a test grid to be O(n)
sizedTorTriGridTD :: Int -> Gen TorTriGridTD
sizedTorTriGridTD n = do
  r0 <- choose (0,n `div` 2)
  let r = 2*r0
  let c = n `div` (2*r + 1)
  let g = TorTriGrid (r, c)
--  r <- choose (0,n)
--  let c = n `div` (2*r + 1)
--  let g = torTriGrid r c
  ps <- chooseIndices g n
  qs <- chooseClosePoints g `suchThat` bothValid
  TorTriGridTD g ps qs <$> arbitrary

instance Arbitrary TorTriGridTD where
  arbitrary = sized sizedTorTriGridTD

torTriGridProperties :: [(String, TorTriGridTD -> Property)]
torTriGridProperties = gridProperties "TorTriGrid"
  ++ finiteGridProperties "TorTriGrid"

torTriGridTests :: [Test]
torTriGridTests = makeTests torTriGridProperties


--
-- Toroidal grids with triangular tiles
--

data YCylTriGridTD =
  YCylTriGridTD YCylTriGrid [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData YCylTriGridTD where
  type BaseGrid YCylTriGridTD = YCylTriGrid
  grid (YCylTriGridTD g _ _ _) = g
  points (YCylTriGridTD _ ps _ _) = ps
  twoClosePoints (YCylTriGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 3)
  direction (YCylTriGridTD _ _ _ d) = d

instance TestDataF YCylTriGridTD where
  maxDistance (YCylTriGridTD g _ _ _) = 2*(r+c) - 3
    where (r, c) = size g
  expectedTileCount (YCylTriGridTD g _ _ _) = 2*r*c
    where (r, c) = size g

-- We want the number of tiles in a test grid to be O(n)
sizedYCylTriGridTD :: Int -> Gen YCylTriGridTD
sizedYCylTriGridTD n = do
  r0 <- choose (0,n `div` 2)
  let r = 2*r0
  let c = n `div` (2*r + 1)
  let g = YCylTriGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g `suchThat` bothValid
  YCylTriGridTD g ps qs <$> arbitrary

instance Arbitrary YCylTriGridTD where
  arbitrary = sized sizedYCylTriGridTD

yCylTriGridProperties :: [(String, YCylTriGridTD -> Property)]
yCylTriGridProperties = gridProperties "YCylTriGrid"
  ++ finiteGridProperties "YCylTriGrid"

yCylTriGridTests :: [Test]
yCylTriGridTests = makeTests yCylTriGridProperties

data XCylTriGridTD =
  XCylTriGridTD XCylTriGrid [(Int,Int)] ((Int,Int),(Int,Int)) TriDirection
  deriving (Show, Read)

instance TestData XCylTriGridTD where
  type BaseGrid XCylTriGridTD = XCylTriGrid
  grid (XCylTriGridTD g _ _ _) = g
  points (XCylTriGridTD _ ps _ _) = ps
  twoClosePoints (XCylTriGridTD _ _ qs _) = qs
  neighbourCountBounds _ = (0, 3)
  direction (XCylTriGridTD _ _ _ d) = d

instance TestDataF XCylTriGridTD where
  maxDistance (XCylTriGridTD g _ _ _) = 2*(r+c) - 3
    where (r, c) = size g
  expectedTileCount (XCylTriGridTD g _ _ _) = 2*r*c
    where (r, c) = size g

-- We want the number of tiles in a test grid to be O(n)
sizedXCylTriGridTD :: Int -> Gen XCylTriGridTD
sizedXCylTriGridTD n = do
  r0 <- choose (0,n `div` 2)
  let r = 2*r0
  let c = n `div` (2*r + 1)
  let g = XCylTriGrid (r, c)
  ps <- chooseIndices g n
  qs <- chooseClosePoints g `suchThat` bothValid
  XCylTriGridTD g ps qs <$> arbitrary

instance Arbitrary XCylTriGridTD where
  arbitrary = sized sizedXCylTriGridTD

xCylTriGridProperties :: [(String, XCylTriGridTD -> Property)]
xCylTriGridProperties = gridProperties "XCylTriGrid"
  ++ finiteGridProperties "XCylTriGrid"

xCylTriGridTests :: [Test]
xCylTriGridTests = makeTests xCylTriGridProperties

test :: Test
test = testGroup "Math.Geometry.Grid.TriangularQC"
  ( unboundedTriGridTests ++ triTriGridTests ++ paraTriGridTests
    ++ rectTriGridTests ++ torTriGridTests ++ yCylTriGridTests
    ++ xCylTriGridTests)


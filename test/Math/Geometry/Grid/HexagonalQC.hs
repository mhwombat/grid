{-# LANGUAGE UnicodeSyntax, FlexibleContexts, ExistentialQuantification,
    TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.Grid.HexagonalQC
  (
    test
  ) where

import Math.Geometry.Grid.HexagonalInternal 
import Math.Geometry.GridInternal 
import Math.Geometry.GridQC

import Prelude hiding (null)
import qualified Prelude as P (null)
import Test.Framework as TF (Test, testGroup)
import Test.QuickCheck 
  (Gen, Arbitrary, arbitrary, sized, choose, Property, vectorOf)

--
-- Unbounded grids with hexagonal tiles
--

data UnboundedHexGridTD = 
  UnboundedHexGridTD [(Int,Int)] ((Int,Int),(Int,Int))
  deriving Show

instance TestData UnboundedHexGridTD where
  type BaseGrid UnboundedHexGridTD = UnboundedHexGrid
  grid _ = UnboundedHexGrid
  points (UnboundedHexGridTD ps _) = ps
  twoClosePoints (UnboundedHexGridTD _ qs) = qs
  neighbourCountBounds _ = (6, 6)

sizedUnboundedHexGridTD ∷ Int → Gen UnboundedHexGridTD
sizedUnboundedHexGridTD n = do
  k <- choose (0,n)
  ps <- vectorOf (k+2) arbitrary :: Gen [(Int,Int)]
  qs <- chooseClosePointsUnbounded
  return $ UnboundedHexGridTD ps qs

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
  HexHexGridTD HexHexGrid [(Int,Int)] ((Int,Int),(Int,Int))
  deriving Show

instance TestData HexHexGridTD where
  type BaseGrid HexHexGridTD = HexHexGrid
  grid (HexHexGridTD g _ _) = g
  points (HexHexGridTD _ ps _) = ps
  twoClosePoints (HexHexGridTD _ _ qs) = qs
  neighbourCountBounds _ = (0, 6)

instance TestDataF HexHexGridTD where
  maxDistance (HexHexGridTD g _ _) = 2*s - 2
    where s = size g
  expectedTileCount (HexHexGridTD g _ _) = 3*s*(s-1) + 1
    where s = size g

instance TestDataB HexHexGridTD where
  expectedBoundaryCount (HexHexGridTD g _ _) = (f . size) g
    where f 0 = 0
          f 1 = 1
          f s = 6*(s-1)

-- We want the number of tiles in a test grid to be O(n)
sizedHexHexGridTD ∷ Int → Gen HexHexGridTD
sizedHexHexGridTD n = do
  let s = isqrt (n `div` 3)
  let g = hexHexGrid s
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  return $ HexHexGridTD g ps qs

instance Arbitrary HexHexGridTD where
  arbitrary = sized sizedHexHexGridTD

hexHexGridProperties :: [(String, HexHexGridTD -> Property)]
hexHexGridProperties = gridProperties "HexHexGrid"
  ++ finiteGridProperties "HexHexGrid"
  ++ boundedGridProperties "HexHexGrid"

hexHexGridTests :: [Test]
hexHexGridTests = makeTests hexHexGridProperties

--
-- Parallelogrammatical hexagonal grids   
--

data ParaHexGridTD = 
  ParaHexGridTD ParaHexGrid [(Int,Int)] ((Int,Int),(Int,Int))
  deriving Show

instance TestData ParaHexGridTD where
  type BaseGrid ParaHexGridTD = ParaHexGrid
  grid (ParaHexGridTD g _ _) = g
  points (ParaHexGridTD _ ps _) = ps 
  twoClosePoints (ParaHexGridTD _ _ qs) = qs
  neighbourCountBounds _ = (0, 6)

instance TestDataF ParaHexGridTD where
  maxDistance (ParaHexGridTD g _ _) = r+c-2
    where (r, c) = size g
  expectedTileCount (ParaHexGridTD g _ _) = r*c
    where (r,c) = size g

instance TestDataB ParaHexGridTD where
  expectedBoundaryCount (ParaHexGridTD g _ _) = 
    (cartesianBoundaryCount . size) g

-- We want the number of tiles in a test grid to be O(n)
sizedParaHexGridTD ∷ Int → Gen ParaHexGridTD
sizedParaHexGridTD n = do
  r ← choose (0,n)
  let c = n `div` (r+1)
  let g = paraHexGrid r c
  ps <- chooseIndices g n
  qs <- chooseClosePoints g
  return $ ParaHexGridTD g ps qs

instance Arbitrary ParaHexGridTD where
  arbitrary = sized sizedParaHexGridTD

paraHexGridProperties :: [(String, ParaHexGridTD -> Property)]
paraHexGridProperties = gridProperties "ParaHexGrid"
  ++ finiteGridProperties "ParaHexGrid"
  ++ boundedGridProperties "ParaHexGrid"

paraHexGridTests :: [Test]
paraHexGridTests = makeTests paraHexGridProperties

test ∷ Test
test = testGroup "Math.Geometry.Grid.HexagonalQC"
  (unboundedHexGridTests ++ hexHexGridTests ++ paraHexGridTests)



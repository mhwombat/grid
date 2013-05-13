{-# LANGUAGE UnicodeSyntax, FlexibleContexts, ExistentialQuantification,
    TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.Grid.OctagonalQC
  (
    test
  ) where

import Math.Geometry.Grid.OctagonalInternal 
import Math.Geometry.GridInternal 
import Math.Geometry.GridQC

import Prelude hiding (null)
import qualified Prelude as P (null)
import Test.Framework as TF (Test, testGroup)
import Test.QuickCheck 
  (Gen, Arbitrary, arbitrary, sized, choose, Property, vectorOf)

--
-- Unbounded grids with octagonal tiles
--

data UnboundedOctGridTD = 
  UnboundedOctGridTD [(Int,Int)] ((Int,Int),(Int,Int))
  deriving Show

instance TestData UnboundedOctGridTD where
  type BaseGrid UnboundedOctGridTD = UnboundedOctGrid
  grid _ = UnboundedOctGrid
  points (UnboundedOctGridTD ps _) = ps
  twoClosePoints (UnboundedOctGridTD _ qs) = qs
  neighbourCountBounds _ = (8, 8)

sizedUnboundedOctGridTD ∷ Int → Gen UnboundedOctGridTD
sizedUnboundedOctGridTD n = do
  k ← choose (0,n)
  ps ← vectorOf (k+2) arbitrary ∷ Gen [(Int,Int)]
  qs ← chooseClosePointsUnbounded
  return $ UnboundedOctGridTD ps qs

instance Arbitrary UnboundedOctGridTD where
  arbitrary = sized sizedUnboundedOctGridTD

unboundedOctGridProperties ∷ [(String, UnboundedOctGridTD → Property)]
unboundedOctGridProperties = gridProperties "UnboundedOctGrid"

unboundedOctGridTests ∷ [Test]
unboundedOctGridTests = makeTests unboundedOctGridProperties

--
-- Rectangular grids with octagonal tiles
--

data RectOctGridTD = 
  RectOctGridTD RectOctGrid [(Int,Int)] ((Int,Int),(Int,Int))
  deriving Show

instance TestData RectOctGridTD where
  type BaseGrid RectOctGridTD = RectOctGrid
  grid (RectOctGridTD g _ _) = g
  points (RectOctGridTD _ ps _) = ps 
  twoClosePoints (RectOctGridTD _ _ qs) = qs
  neighbourCountBounds _ = (0, 8)

instance TestDataF RectOctGridTD where
  maxDistance (RectOctGridTD g _ _) = (max r c) - 1
    where (r, c) = size g
  expectedTileCount (RectOctGridTD g _ _) = r*c
    where (r,c) = size g

instance TestDataB RectOctGridTD where
  expectedBoundaryCount (RectOctGridTD g _ _) = 
    (cartesianBoundaryCount . size) g

-- We want the number of tiles in a test grid to be O(n)
sizedRectOctGridTD ∷ Int → Gen RectOctGridTD
sizedRectOctGridTD n = do
--  let n' = min n 12 -- calculation time for these grids grows quickly!
--  r ← choose (0,n')
--  let c = n' `div` (r+1)
  r ← choose (0,n)
  let c = n `div` (r+1)
  let g = rectOctGrid r c
  ps ← chooseIndices g n
  qs ← chooseClosePoints g
  return $ RectOctGridTD g ps qs

instance Arbitrary RectOctGridTD where
  arbitrary = sized sizedRectOctGridTD

rectOctGridProperties ∷ [(String, RectOctGridTD → Property)]
rectOctGridProperties = gridProperties "RectOctGrid"
  ++ finiteGridProperties "RectOctGrid"
  ++ boundedGridProperties "RectOctGrid"

rectOctGridTests ∷ [Test]
rectOctGridTests = makeTests rectOctGridProperties


--
-- Toroidal grids with octagonal tiles
--

data TorOctGridTD = 
  TorOctGridTD TorOctGrid [(Int,Int)] ((Int,Int),(Int,Int))
  deriving Show

instance TestData TorOctGridTD where
  type BaseGrid TorOctGridTD = TorOctGrid
  grid (TorOctGridTD g _ _) = g
  points (TorOctGridTD _ ps _) = ps 
  twoClosePoints (TorOctGridTD _ _ qs) = qs
  neighbourCountBounds _ = (0, 8)

instance TestDataF TorOctGridTD where
  maxDistance (TorOctGridTD g _ _) = min r c + abs (r-c)
    where (r, c) = size g
  expectedTileCount (TorOctGridTD g _ _) = r*c
    where (r,c) = size g

-- We want the number of tiles in a test grid to be O(n)
sizedTorOctGridTD ∷ Int → Gen TorOctGridTD
sizedTorOctGridTD n = do
  r ← choose (0,n)
  let c = n `div` (r+1)
  let g = torOctGrid r c
  ps ← chooseIndices g n
  qs ← chooseClosePoints g
  return $ TorOctGridTD g ps qs

instance Arbitrary TorOctGridTD where
  arbitrary = sized sizedTorOctGridTD

torOctGridProperties ∷ [(String, TorOctGridTD → Property)]
torOctGridProperties = gridProperties "TorOctGrid"
  ++ finiteGridProperties "TorOctGrid"

torOctGridTests ∷ [Test]
torOctGridTests = makeTests torOctGridProperties



--TODO redo these

--prop_RectOctGrid_num_min_paths_correct ∷ 
--  RectOctGrid → Int → Int → Property
--prop_RectOctGrid_num_min_paths_correct g i j = nonNull g ==>
--  minPathCount g a b ≡
--    if a ≡ b then 1 else minPathCount2 g att b
--    where a = g `pointAt` i
--          b = g `pointAt` j
--          att = adjacentTilesToward g a b


test ∷ Test
test = testGroup "Math.Geometry.Grid.OctagonalQC"
  (unboundedOctGridTests ++ rectOctGridTests ++ torOctGridTests)



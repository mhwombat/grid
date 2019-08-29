------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid.GridQC
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ExistentialQuantification, TypeFamilies,
    MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.GridQC where

import Math.Geometry.GridInternal 

import Prelude hiding (null)
import qualified Prelude as P (null)
import Data.List (delete, nub, sort)
import Data.Maybe (isJust, fromJust)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck 
  ((==>), Gen, Arbitrary, arbitrary, choose, Property, property,
    vectorOf, elements)

-- | @'isqrt' n@ returns the greatest integer not greater than the square root 
--   of @n@.
isqrt :: Int -> Int
isqrt n = (floor . sqrt) n'
  where n' = fromIntegral n :: Float

-- Given an arbitrary integer, select a corresponding point in the grid.
pointAt :: Grid g => g -> Int -> Index g
pointAt g i = indices g !! (i `mod` n)
  where n = (length . indices) g

minPathCount
  :: (Eq (Index g), Grid g) => g -> Index g -> Index g -> Int
minPathCount g a b = length . minimalPaths g a $ b

minPathCount2
  :: (Eq (Index g), Grid g) => g -> [Index g] -> Index g -> Int
minPathCount2 g as b = sum . map (\x -> minPathCount g x b) $ as

cartesianBoundaryCount :: (Eq a, Num a) => (a, a) -> a
cartesianBoundaryCount (0,_) = 0
cartesianBoundaryCount (_,0) = 0
cartesianBoundaryCount (1,c) = c
cartesianBoundaryCount (r,1) = r
cartesianBoundaryCount (r,c) = 2*(r+c) - 4

involves :: Eq a => (a, a) -> a -> Bool
involves (a, b) c = c == a || c == b

chooseIndices :: Grid g => g -> Int -> Gen [Index g]
chooseIndices g n = do
  k <- choose (0,n)
  if null g 
    then return [] 
    else vectorOf (k+2) (elements . indices $ g)

chooseClosePointsUnbounded :: Gen ((Int, Int), (Int, Int))
chooseClosePointsUnbounded = do
  (x1,y1) <- arbitrary
  x2 <- choose (x1-2,x1+2)
  y2 <- choose (y1-2,y1+2)
  return ((x1,y1), (x2,y2))

chooseClosePoints :: Grid g => g -> Gen (Index g, Index g)
chooseClosePoints g = do
  a <- elements . indices $ g
  b <- elements . filter (\b -> distance g a b < 6) . indices $ g
  return (a, b)

makeTests :: (Arbitrary t, Show t) => [(String, t -> Property)] -> [Test]
makeTests ts = map (\(s,t) -> testProperty s t) ts

--
-- Tests that should apply to and are identical for all grids
--

class TestData t where
  type BaseGrid t
  grid :: t -> BaseGrid t
  points :: t -> [Index (BaseGrid t)]
  neighbourCountBounds :: t -> (Int, Int)
  twoClosePoints :: t -> (Index (BaseGrid t),Index (BaseGrid t))
  direction :: t -> Direction (BaseGrid t)

prop_indices_are_contained :: (TestData t, Grid (BaseGrid t), 
  Eq (Index (BaseGrid t))) => t -> Property
prop_indices_are_contained t = nonNull g ==> g `contains` a
  where g = grid t
        (a:_) = points t

prop_distance_reflexive :: (TestData t, Grid (BaseGrid t)) => t -> Property
prop_distance_reflexive t = nonNull g ==> distance g a a == 0
  where g = grid t
        (a:_) = points t

prop_distance_symmetric :: (TestData t, Grid (BaseGrid t)) => t -> Property
prop_distance_symmetric t = 
  nonNull g ==> distance g a b == distance g b a
  where g = grid t
        (a:b:_) = points t

prop_custom_MinDistance_eq_default 
  :: (TestData t, Grid (BaseGrid t)) => t -> Property
prop_custom_MinDistance_eq_default t = nonNull g ==> 
  minDistance g bs a == defaultMinDistance g bs a
  where g = grid t
        (a:bs) = points t

-- "cw" = "consistent with"

prop_minDistance_cw_distance :: (TestData t, Grid (BaseGrid t)) => t -> Property
prop_minDistance_cw_distance t = 
  nonNull g && (not . P.null) bs ==> 
    minDistance g (b:bs) a <= distance g b a
  where g = grid t
        (a:b:bs) = points t

prop_neighbour_count_in_bounds
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_neighbour_count_in_bounds t = nonNull g ==> 
  nMin <= n && n <= nMax
  where g = grid t
        (a:_) = points t
        n = length . neighbours g $ a
        (nMin, nMax) = neighbourCountBounds t

prop_neighbours_are_adjacent
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)))
    => t -> Property
prop_neighbours_are_adjacent t = nonNull g  ==> 
    and (map (isAdjacent g a) ns)
  where g = grid t
        (a:_) = points t
        ns = neighbours g a

prop_adjacentTilesToward_moves_closer
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)))
    => t -> Property
prop_adjacentTilesToward_moves_closer t = nonNull g && a /= b ==> 
    and (map (< d) ns)
  where g = grid t
        (a:b:_) = points t
        d = distance g a b
        ns = nub $ map (\x -> distance g x b) $ adjacentTilesToward g a b

prop_minimal_paths_have_min_length
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)))
    => t -> Property
prop_minimal_paths_have_min_length t = nonNull g ==> ns == [d+1]
  where g = grid t
        (a,b) = twoClosePoints t
        d = distance g a b
        ns = nub . map length . minimalPaths g a $ b

prop_minimal_paths_are_valid
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)))
    => t -> Property
prop_minimal_paths_are_valid t = nonNull g ==> 
    and $ map (subsequentTilesInPathAreAdjacent g) $ minimalPaths g a b
  where g = grid t
        (a,b) = twoClosePoints t

subsequentTilesInPathAreAdjacent 
  :: (Grid g, Eq (Index g)) => g -> [Index g] -> Bool
subsequentTilesInPathAreAdjacent _ [] = True
subsequentTilesInPathAreAdjacent g [x] = g `contains` x
subsequentTilesInPathAreAdjacent g (a:b:xs) = 
  isAdjacent g a b && subsequentTilesInPathAreAdjacent g (b:xs)

prop_neighbour_cw_directionTo
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)), 
    Eq (Direction (BaseGrid t)))
    => t -> Property
prop_neighbour_cw_directionTo t = nonNull g && a /= b && isJust n ==> 
    (fromJust n) `elem` nextSteps
  where n = neighbour g a d
        g = grid t
        (a,b) = twoClosePoints t
        d = head . directionTo g a $ b
        nextSteps = map (!!1) . minimalPaths g a $ b

prop_custom_adjacentTilesToward_eq_default
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_adjacentTilesToward_eq_default t = nonNull g ==>
  adjacentTilesToward g a b == defaultAdjacentTilesToward g a b
  where g = grid t
        (a:b:_) = points t

prop_custom_neighboursOfSet_eq_default
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_neighboursOfSet_eq_default t = nonNull g ==>
  neighboursOfSet g as == defaultNeighboursOfSet g as
  where g = grid t
        as = points t

prop_custom_neighboursOfSet_cw_minDistance
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_neighboursOfSet_cw_minDistance t = nonNull g ==>
  a `elem` (neighboursOfSet g bs) || minDistance g bs a /= 1 
  where g = grid t
        (a:bs) = points t

gridProperties 
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)),
    Ord (Index (BaseGrid t)), Eq (Direction (BaseGrid t))) 
      => String -> [(String, t -> Property)]
gridProperties s = 
  [
    ("prop_indices_are_contained: " ++ s, prop_indices_are_contained),
    ("prop_distance_reflexive: " ++ s, prop_distance_reflexive),
    ("prop_distance_symmetric: " ++ s, prop_distance_symmetric),
    ("prop_custom_MinDistance_eq_default: " ++ s, prop_custom_MinDistance_eq_default),
    ("prop_minDistance_cw_distance: " ++ s, prop_minDistance_cw_distance),
    ("prop_neighbour_count_in_bounds: " ++ s, prop_neighbour_count_in_bounds),
    ("prop_neighbours_are_adjacent: " ++ s, prop_neighbours_are_adjacent),
    ("prop_adjacentTilesToward_moves_closer: " ++ s, prop_adjacentTilesToward_moves_closer),
    ("prop_minimal_paths_have_min_length: " ++ s, prop_minimal_paths_have_min_length),
    ("prop_minimal_paths_are_valid: " ++ s, prop_minimal_paths_are_valid),
    ("prop_neighbour_cw_directionTo: " ++ s, prop_neighbour_cw_directionTo),
    ("prop_custom_adjacentTilesToward_eq_default: " ++ s, prop_custom_adjacentTilesToward_eq_default),
    ("prop_custom_neighboursOfSet_eq_default: " ++ s, prop_custom_neighboursOfSet_eq_default),
    ("prop_custom_neighboursOfSet_cw_minDistance: " ++ s, prop_custom_neighboursOfSet_cw_minDistance)
  ]

--
-- Tests that should apply to and are identical for all finite grids
--

class TestDataF t where
  expectedTileCount :: t -> Int
  maxDistance :: t -> Int

prop_tile_count_correct
  :: (TestData t, TestDataF t, Grid (BaseGrid t))
    => t -> Property
prop_tile_count_correct t = nonNull g ==>
  tileCount g == expectedTileCount t 
  where g = grid t

prop_custom_tileCount_eq_default 
  :: (TestData t, Grid (BaseGrid t)) => t -> Property
prop_custom_tileCount_eq_default t = nonNull g ==> 
  tileCount g == defaultTileCount g
  where g = grid t

prop_distance_in_bounds
  :: (TestData t, TestDataF t, Grid (BaseGrid t))
    => t -> Property
prop_distance_in_bounds t = nonNull g ==> 
  0 <= n && n <= maxDistance t
  where g = grid t
        (a:b:_) = points t
        n = distance g a b

prop_neighbours_cw_viewpoint 
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_neighbours_cw_viewpoint t = nonNull g ==> 
  sort (delete a (neighbours g a)) == sort expected
  where g = grid t
        (a:_) = points t
        expected = map fst $ filter (\p -> 1 == snd p) $ viewpoint g a
-- Note: In a small but unbounded grid, a tile can be its own neighbour.
-- However, when we calculate the distance between a tile and itself, we
-- get 0, not 1. That's why we have to delete the tile from its list 
-- before comparing to the result from the neighbours function.

prop_custom_edges_eq_default 
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)), 
    Ord (Index (BaseGrid t))) => t -> Property
prop_custom_edges_eq_default t = nonNull g ==> 
  sort (edges g) == sort (defaultEdges g)
  where g = grid t

prop_edges_cw_neighbours
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_edges_cw_neighbours t = nonNull g ==> 
  sort (neighbours g a) == sort expected
  where g = grid t
        (a:_) = points t
        nEdges = filter (`involves` a) $ edges g
        expected = map f nEdges
        f (b,c) = if a == b then c else b

prop_edges_are_adjacent
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_edges_are_adjacent t = property $ all f $ edges g
  where g = grid t
        f (a, b) = isAdjacent g a b

-- This test is too slow, even for finite grids.
-- TODO: Try a better implementation of defaultMinimalPaths?
prop_custom_minimalPaths_eq_default
  :: (TestData t, Grid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_minimalPaths_eq_default t = nonNull g ==>
  sort (minimalPaths g a b) == sort(defaultMinimalPaths g a b)
  where g = grid t
        (a:b:_) = points t

prop_distance_le_maxPossibleDistance
  :: (TestData t, FiniteGrid (BaseGrid t))
    => t -> Property
prop_distance_le_maxPossibleDistance t = nonNull g ==>
  distance g a b <= maxPossibleDistance g
  where g = grid t
        (a:b:_) = points t

prop_maxPossibleDistance_occurs
  :: (TestData t, FiniteGrid (BaseGrid t),
      Ord (Index (BaseGrid t)))
    => t -> Property
prop_maxPossibleDistance_occurs t = nonNull g ==>
  dMax `elem` [distance g x y | x <- indices g, y <- (reverse . sort $ indices g)]
  -- If we process x and y in opposite orders, we're more likely to find
  -- the furthest two points in the grid early on.
  where g = grid t
        dMax = maxPossibleDistance g

finiteGridProperties 
  :: (TestData t, TestDataF t, FiniteGrid (BaseGrid t),
    Eq (Index (BaseGrid t)), Ord (Index (BaseGrid t))) 
    => String -> [(String, t -> Property)]
finiteGridProperties s = 
  [
    ("prop_tile_count_correct: " ++ s, prop_tile_count_correct),
    ("prop_custom_tileCount_eq_default: " ++ s, prop_custom_tileCount_eq_default),
    ("prop_distance_in_bounds: " ++ s, prop_distance_in_bounds),
    ("prop_neighbours_cw_viewpoint: " ++ s, prop_neighbours_cw_viewpoint),
    ("prop_custom_edges_eq_default: " ++ s, prop_custom_edges_eq_default),
    ("prop_edges_cw_neighbours: " ++ s, prop_edges_cw_neighbours),
    ("prop_edges_are_adjacent: " ++ s, prop_edges_are_adjacent),
--    ("prop_custom_minimalPaths_eq_default: " ++ s, prop_custom_minimalPaths_eq_default)
    ("prop_distance_le_maxPossibleDistance: " ++ s, prop_distance_le_maxPossibleDistance),
    ("prop_maxPossibleDistance_occurs: " ++ s, prop_maxPossibleDistance_occurs)
  ]

--
-- Tests that should apply to and are identical for all bounded grids
--

class TestDataB t where
  expectedBoundaryCount :: t -> Int

prop_custom_boundary_eq_default
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_boundary_eq_default t = nonNull g ==>
  sort (boundary g) == sort (defaultBoundary g)
  where g = grid t

prop_boundary_count_correct
  :: (TestData t, TestDataB t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_boundary_count_correct t = nonNull g ==>
  (length . boundary) g == expectedBoundaryCount t 
  where g = grid t

prop_grid_and_boundary_are_both_null_or_not 
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_grid_and_boundary_are_both_null_or_not t = property $
  (P.null . boundary) g == null g
  where g = grid t

prop_boundary_in_grid
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_boundary_in_grid t = property $
  all (g `contains`) . boundary $ g
  where g = grid t

prop_boundary_tiles_have_fewer_neighbours
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_boundary_tiles_have_fewer_neighbours t = nonNull g ==>
  g `numNeighbours` b <= g `numNeighbours` a
  where g = grid t
        (a:_) = points t
        (b:_) = boundary g

prop_custom_isBoundary_eq_default
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_isBoundary_eq_default t = nonNull g ==>
  isBoundary g a == defaultIsBoundary g a
  where g = grid t
        (a:_) = points t

prop_custom_isCentre_eq_default
  :: (TestData t, BoundedGrid (BaseGrid t), Eq (Index (BaseGrid t)))
    => t -> Property
prop_custom_isCentre_eq_default t = nonNull g ==>
  isCentre g a == defaultIsCentre g a
  where g = grid t
        (a:_) = points t

prop_custom_neighbours_eq_default 
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)),
      Ord (Index (BaseGrid t)))
     => t -> Property
prop_custom_neighbours_eq_default t = nonNull g ==> 
  sort (neighbours g a) == sort (defaultNeighbours g a)
  where g = grid t
        (a:_) = points t

prop_custom_neighbour_eq_default 
  :: (TestData t, Grid (BaseGrid t), Eq (Index (BaseGrid t)), (Eq (Direction (BaseGrid t))))
     => t -> Property
prop_custom_neighbour_eq_default t = nonNull g ==> 
  neighbour g a d == defaultNeighbour g a d
  where g = grid t
        (a:_) = points t
        d = direction t

prop_custom_isAdjacent_eq_default
  :: (TestData t, Grid (BaseGrid t))
    => t -> Property
prop_custom_isAdjacent_eq_default t = nonNull g ==>
  isAdjacent g a b == defaultIsAdjacent g a b
  where g = grid t
        (a:b:_) = points t

boundedGridProperties 
  :: (TestData t, TestDataB t, BoundedGrid (BaseGrid t),
    Eq (Index (BaseGrid t)), Ord (Index (BaseGrid t)),
    Eq (Direction (BaseGrid t))) 
    => String -> [(String, t -> Property)]
boundedGridProperties s = 
  [
    ("prop_custom_boundary_eq_default: " ++ s, prop_custom_boundary_eq_default),
    ("prop_boundary_count_correct: " ++ s, prop_boundary_count_correct),
    ("prop_grid_and_boundary_are_both_null_or_not: " ++ s, prop_grid_and_boundary_are_both_null_or_not),
    ("prop_boundary_in_grid: " ++ s, prop_boundary_in_grid),
    ("prop_boundary_tiles_have_fewer_neighbours: " ++ s, prop_boundary_tiles_have_fewer_neighbours),
    ("prop_custom_isBoundary_eq_default: " ++ s, prop_custom_isBoundary_eq_default),
    ("prop_custom_isCentre_eq_default: " ++ s, prop_custom_isBoundary_eq_default),
    ("prop_custom_neighbours_eq_default: " ++ s, prop_custom_neighbours_eq_default),
    ("prop_custom_neighbour_eq_default: " ++ s, prop_custom_neighbour_eq_default),
    ("prop_custom_isAdjacent_eq_default: " ++ s, prop_custom_isAdjacent_eq_default)
  ]

--
-- These properties won't work for triangular grids.
-- They probably only work on grids where all the tiles have the same
-- shape/orientation.
--

prop_custom_centre_eq_default
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_custom_centre_eq_default t = nonNull g ==>
  sort(centre g) == sort (defaultCentre g)
  where g = grid t

prop_centres_equidistant_from_boundary
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_centres_equidistant_from_boundary t = nonNull g ==>
  (length . nub . map (minDistance g bs)) cs == 1
  where g = grid t
        bs = boundary g
        cs = centre g

prop_centres_farthest_from_boundary
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t)))
    => t -> Property
prop_centres_farthest_from_boundary t = 
  nonNull g && (not . isCentre g) a ==>
    minDistance g bs a <= minDistance g bs c
  where g = grid t
        (a:_) = points t
        (c:_) = centre g
        bs = boundary g

boundedGridProperties2
  :: (TestData t, BoundedGrid (BaseGrid t), Ord (Index (BaseGrid t))) 
    => String -> [(String, t -> Property)]
boundedGridProperties2 s = 
  [
   ("prop_custom_centre_eq_default: " ++ s, prop_custom_centre_eq_default),
   ("prop_centres_equidistant_from_boundary: " ++ s, prop_centres_equidistant_from_boundary),
   ("prop_centres_farthest_from_boundary: " ++ s, prop_centres_farthest_from_boundary)
  ]


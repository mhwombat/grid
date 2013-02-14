{-# LANGUAGE UnicodeSyntax, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.GridQC
  (
    test
  ) where

import Math.Geometry.GridInternal 

import Data.Eq.Unicode ((≡), (≠))
import Data.List (nub, sort)
import Data.Ord.Unicode ((≤))
import qualified Math.Combinatorics.Exact.Binomial as M (choose)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck 
  ((==>), Gen, Arbitrary, arbitrary, sized, choose, Property, property)

-- | @'isqrt' n@ returns the greatest integer not greater than the square root 
--   of @n@.
isqrt ∷ Int → Int
isqrt n = (floor . sqrt) n'
  where n' = fromIntegral n ∷ Float

-- Given an arbitrary integer, select a corresponding point in the grid.
pointAt ∷ Grid g s x ⇒ g → Int → x
pointAt g i = indices g !! (i `mod` n)
  where n = (length . indices) g

--
-- Tests that should apply to and are identical for all grids
--

prop_distance_reflexive ∷ Grid g s x ⇒ g → Int → Property
prop_distance_reflexive g i = nonEmpty g ==> distance g a a ≡ 0
  where a = g `pointAt` i

prop_distance_symmetric ∷ Grid g s x ⇒ g → Int → Int → Property
prop_distance_symmetric g i j = 
  nonEmpty g ==> distance g a b ≡ distance g b a
  where a = g `pointAt` i
        b = g `pointAt` j

-- "cw" = "consistent with"

prop_minDistance_cw_distance ∷ Grid g s x ⇒ g → Int → [Int] → Property
prop_minDistance_cw_distance g i js = 
  nonEmpty g && (not . null) js ==> 
    minDistance g (b:bs) a ≤ distance g b a
  where a = g `pointAt` i
        (b:bs) = map (g `pointAt`) js

prop_neighbours_cw_viewpoint ∷ (Grid g s x, Ord x) ⇒ g → Int → Property
prop_neighbours_cw_viewpoint g i = n > 0 ==> 
  sort (neighbours g a) ≡ sort expected
    where n = (length . indices) g
          a = indices g !! (i `mod` n) -- make sure point is in grid
          expected = map fst $ filter (\p → 1 ≡ snd p) $ viewpoint g a

prop_edges_cw_neighbours ∷ (Grid g s x, Ord x) ⇒ g → Int → Property
prop_edges_cw_neighbours g i = n > 0 ==> 
  sort (neighbours g a) ≡ sort expected
    where n = (length . indices) g
          a = indices g !! (i `mod` n) -- make sure point is in grid
          nEdges = filter (`involves` a) $ edges g
          expected = filter (≠ a) $ nub $ map fst nEdges ++ map snd nEdges

involves ∷ Eq a ⇒ (a, a) → a → Bool
involves (a, b) c = c ≡ a || c ≡ b

prop_edges_are_adjacent ∷ (Grid g s x, Ord x) ⇒ g → Property
prop_edges_are_adjacent g = property $ all f $ edges g
  where f (a, b) = isAdjacent g a b

prop_adjacentTilesToward_moves_closer ∷ Grid g s x ⇒ g → Int → Int → Property
prop_adjacentTilesToward_moves_closer g i j = nonEmpty g && a ≠ b ==> 
    ns ≡ [d-1]
  where a = g `pointAt` i
        b = g `pointAt` j
        d = distance g a b
        ns = nub $ map (\x → distance g x b) $ adjacentTilesToward g a b

prop_minimal_paths_have_min_length ∷ Grid g s x ⇒ g → Int → Int → Property
prop_minimal_paths_have_min_length g i j = nonEmpty g ==> ns ≡ [d+1]
  where a = g `pointAt` i
        b = g `pointAt` j
        d = distance g a b
        ns = nub $ map length $ minimalPaths g a b

prop_minimal_paths_are_valid ∷ Grid g s x ⇒ g → Int → Int → Property
prop_minimal_paths_are_valid g i j = nonEmpty g ==> 
    and $ map (subsequentTilesInPathAreAdjacent g) $ minimalPaths g a b
  where a = g `pointAt` i
        b = g `pointAt` j

subsequentTilesInPathAreAdjacent ∷ Grid g s x ⇒ g → [x] → Bool
subsequentTilesInPathAreAdjacent _ [] = True
subsequentTilesInPathAreAdjacent g [x] = x `elem` indices g
subsequentTilesInPathAreAdjacent g (a:b:xs) = 
  isAdjacent g a b && subsequentTilesInPathAreAdjacent g (b:xs)

--
-- Tests that should apply to and are identical for all bounded grids
--

prop_grid_and_boundary_are_both_null_or_not 
  ∷ BoundedGrid g s x ⇒ g → Property
prop_grid_and_boundary_are_both_null_or_not g = property $
  (null . boundary) g ≡ empty g

prop_boundary_in_grid ∷ BoundedGrid g s x ⇒ g → Property
prop_boundary_in_grid g = property $
  all (g `contains`) . boundary $ g

prop_centres_equidistant_from_boundary ∷ BoundedGrid g s x ⇒ g → Property
prop_centres_equidistant_from_boundary g = nonEmpty g ==>
  (length . nub . map (minDistance g bs)) cs ≡ 1
  where bs = boundary g
        cs = centre g

-- Note: We only need to test one of the centres, because the previous
-- test proves they are all equidistant from the boundary.
prop_centres_farthest_from_boundary ∷ BoundedGrid g s x ⇒ g → Int → Property
prop_centres_farthest_from_boundary g i = 
  nonEmpty g && (not . isCentre g) a ==>
    minDistance g bs a ≤ minDistance g bs c
  where a = g `pointAt` i
        (c:_) = centre g
        bs = boundary g

--
-- Triangular grids with triangular tiles
--

-- We want the number of tiles in a test grid to be ~ n
sizedTriTriGrid ∷ Int → Gen TriTriGrid
sizedTriTriGrid n = return $ triTriGrid (2 * isqrt n)

instance Arbitrary TriTriGrid where
  arbitrary = sized sizedTriTriGrid
  
prop_TriTriGrid_tile_count_correct ∷ TriTriGrid → Property
prop_TriTriGrid_tile_count_correct g = property $ 
  (length . indices) g ≡ if s ≤ 0 then 0 else s*s
    where s = size g

prop_TriTriGrid_distance_in_bounds ∷ TriTriGrid → Int → Int → Property
prop_TriTriGrid_distance_in_bounds g i j = nonEmpty g ==> 
  distance g a b ≤ 2*(s-1)
    where s = size g
          a = g `pointAt` i
          b = g `pointAt` j

-- If the ordering produced by triTriGrid is ever changed, this property
-- may need to be changed too. It relies on the first and last elements being
-- at corners.
prop_TriTriGrid_distance_edge_to_edge ∷ TriTriGrid → Property
prop_TriTriGrid_distance_edge_to_edge g = s > 0 ==> distance g a b ≡ 2*(s-1)
  where ps = indices g
        a = head ps
        b = last ps
        s = size g

prop_TriTriGrid_neighbour_count_in_bounds ∷ TriTriGrid → Int → Property
prop_TriTriGrid_neighbour_count_in_bounds g i = nonEmpty g ==>
  if tileCount g ≡ 1
    then length (neighbours g x) ≡ 0
    else length (neighbours g x) `elem` [1,2,3]
  where x = g `pointAt` i

prop_TriTriGrid_boundary_count_correct ∷ TriTriGrid → Property
prop_TriTriGrid_boundary_count_correct g = property $
  (length . boundary) g ≡ (f . size) g
  where f 0 = 0
        f 1 = 1
        f s = 3*(s-1)

prop_TriTriGrid_boundary_tiles_have_fewer_neighbours ∷ TriTriGrid → Property
prop_TriTriGrid_boundary_tiles_have_fewer_neighbours g = property $
  all (3>) . map (numNeighbours g) . boundary $ g

--
-- Parallelogram-shaped grids with triangular tiles
--

-- We want the number of tiles in a test grid to be ~ n
sizedParaTriGrid ∷ Int → Gen ParaTriGrid
sizedParaTriGrid n = do
  r ← choose (0,n)
  let c = n `div` (2*r + 1)
  return $ paraTriGrid r c

instance Arbitrary ParaTriGrid where
  arbitrary = sized sizedParaTriGrid

prop_ParaTriGrid_tile_count_correct ∷ ParaTriGrid → Property
prop_ParaTriGrid_tile_count_correct g = property $ 
  tileCount g ≡ if r ≤ 0 || c ≤ 0 then 0 else 2*r*c
    where (r, c) = size g

prop_ParaTriGrid_distance_in_bounds ∷ ParaTriGrid → Int → Int → Property
prop_ParaTriGrid_distance_in_bounds g i j = nonEmpty g ==> 
  distance g a b ≤ 2*(r+c) - 3
    where (r, c) = size g
          a = g `pointAt` i
          b = g `pointAt` j

-- If the ordering produced by paraTriGrid is ever changed, this
-- property may need to be changed too. It relies on the first and last 
-- elements being at corners.
prop_ParaTriGrid_distance_corner_to_corner ∷ ParaTriGrid → Property
prop_ParaTriGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance g a b ≡ 2*(r+c) - 3
    where ps = indices g
          a = head ps
          b = last ps
          (r, c) = size g

prop_ParaTriGrid_neighbour_count_in_bounds ∷ ParaTriGrid → Int → Property
prop_ParaTriGrid_neighbour_count_in_bounds g i = nonEmpty g ==>
  if tileCount g ≡ 1
    then length (neighbours g x) ≡ 0
    else length (neighbours g x) `elem` [1,2,3]
  where x = g `pointAt` i

prop_ParaTriGrid_boundary_count_correct ∷ ParaTriGrid → Property
prop_ParaTriGrid_boundary_count_correct g = property $
  (length . boundary) g ≡ (f . size) g
  where f (0,_) = 0
        f (_,0) = 0
        f (1,c) = 2*c
        f (r,1) = 2*r
        f (r,c) = 2*(r+c-1)

prop_ParaTriGrid_boundary_tiles_have_fewer_neighbours ∷ ParaTriGrid → Property
prop_ParaTriGrid_boundary_tiles_have_fewer_neighbours g = property $
  all (3>) . map (numNeighbours g) . boundary $ g

--
-- Rectangular grids with square tiles
--

-- We want the number of tiles in a test grid to be ~ n
sizedRectSquareGrid ∷ Int → Gen RectSquareGrid
sizedRectSquareGrid n = do
  r ← choose (0,n)
  let c = n `div` (r+1)
  return $ rectSquareGrid r c

instance Arbitrary RectSquareGrid where
  arbitrary = sized sizedRectSquareGrid

prop_RectSquareGrid_tile_count_correct ∷ RectSquareGrid → Property
prop_RectSquareGrid_tile_count_correct g = property $ 
  tileCount g ≡ if r ≤ 0 || c ≤ 0 then 0 else r*c
    where (r, c) = size g

prop_RectSquareGrid_distance_in_bounds ∷ RectSquareGrid → Int → Int → Property
prop_RectSquareGrid_distance_in_bounds g i j = nonEmpty g ==>
  distance g a b ≤ r + c - 2
    where (r, c) = size g
          a = g `pointAt` i
          b = g `pointAt` j

-- If the ordering produced by rectSquareGrid is ever changed, this
-- property may need to be changed too. It relies on the first and last 
-- elements being at opposite corners.
prop_RectSquareGrid_distance_corner_to_corner ∷ RectSquareGrid → Property
prop_RectSquareGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance g a b ≡ r + c - 2
    where (r, c) = size g
          ps = indices g
          a = head ps
          b = last ps

prop_RectSquareGrid_neighbour_count_in_bounds ∷ 
  RectSquareGrid → Int → Property
prop_RectSquareGrid_neighbour_count_in_bounds g i = nonEmpty g ==> f
  where x = g `pointAt` i
        neighbourCount = length (neighbours g x)
        (r, c) = size g
        f | tileCount g ≡ 1 = neighbourCount ≡ 0
          | r ≡ 1 || c ≡ 1  = neighbourCount `elem` [1,2]
          | otherwise       = neighbourCount `elem` [2,3,4]

prop_RectSquareGrid_num_min_paths_correct ∷ 
  RectSquareGrid → Int → Int → Property
prop_RectSquareGrid_num_min_paths_correct g i j = nonEmpty g ==>
  length (minimalPaths g a b) ≡ M.choose (deltaX+deltaY) deltaX
    where a = g `pointAt` i
          b = g `pointAt` j
          deltaX = abs $ fst b - fst a
          deltaY = abs $ snd b - snd a

prop_RectSquareGrid_boundary_count_correct ∷ RectSquareGrid → Property
prop_RectSquareGrid_boundary_count_correct g = property $
  (length . boundary) g ≡ (cartesianBoundaryCount . size) g

cartesianBoundaryCount ∷ (Eq a, Num a) ⇒ (a, a) → a
cartesianBoundaryCount (0,_) = 0
cartesianBoundaryCount (_,0) = 0
cartesianBoundaryCount (1,c) = c
cartesianBoundaryCount (r,1) = r
cartesianBoundaryCount (r,c) = 2*(r+c) - 4

prop_RectSquareGrid_boundary_tiles_have_fewer_neighbours ∷ RectSquareGrid → Property
prop_RectSquareGrid_boundary_tiles_have_fewer_neighbours g = property $
  all (4>) . map (numNeighbours g) . boundary $ g


--
-- Toroidal grids with square-ish tiles
--

-- We want the number of tiles in a test grid to be ~ n
sizedTorSquareGrid ∷ Int → Gen TorSquareGrid
sizedTorSquareGrid n = do
  r ← choose (0,n)
  let c = n `div` (r+1)
  return $ torSquareGrid r c

instance Arbitrary TorSquareGrid where
  arbitrary = sized sizedTorSquareGrid

prop_TorSquareGrid_tile_count_correct ∷ TorSquareGrid → Property
prop_TorSquareGrid_tile_count_correct g = property $  
  tileCount g ≡ if r ≤ 0 || c ≤ 0 then 0 else r*c
    where (r, c) = size g

prop_TorSquareGrid_distance_in_bounds ∷ TorSquareGrid → Int → Int → Property
prop_TorSquareGrid_distance_in_bounds g i j = nonEmpty g ==>
  distance g a b ≤ (r+c) `div` 2
    where (r, c) = size g
          a = g `pointAt` i
          b = g `pointAt` j

-- If the ordering produced by torSquareGrid is ever changed, this property
-- may need to be changed too.
prop_TorSquareGrid_distance_corner_to_corner ∷ TorSquareGrid → Property
prop_TorSquareGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance g a b ≡ f
    where (r, c) = size g
          ps = indices g
          a = head ps
          b = last ps
          f | r ≡ 1 && c ≡ 1 = 0 -- zero-size torus
            | r ≡ 1 || c ≡ 1 = 1 -- a and b are the same
            | otherwise      = 2

prop_TorSquareGrid_neighbour_count_in_bounds ∷ TorSquareGrid → Int → Property
prop_TorSquareGrid_neighbour_count_in_bounds g i = nonEmpty g ==> f
  where x = g `pointAt` i
        neighbourCount = length (neighbours g x)
        (r, c) = size g
        f | tileCount g ≡ 1 = neighbourCount ≡ 0
          | r ≡ 1 || c ≡ 1  = neighbourCount `elem` [1,2]
          | otherwise       = neighbourCount `elem` [2,3,4]

--
-- Circular hexagonal grids   
--

-- We want the number of tiles in a test grid to be ~ n
sizedHexHexGrid ∷ Int → Gen HexHexGrid
sizedHexHexGrid n = return $ hexHexGrid s
  where s = isqrt (n `div` 3)

instance Arbitrary HexHexGrid where
  arbitrary = sized sizedHexHexGrid

prop_HexHexGrid_tile_count_correct ∷ HexHexGrid → Property
prop_HexHexGrid_tile_count_correct g = property $ 
  (length . indices) g ≡ if s ≤ 0 then 0 else 3*s*(s-1) + 1
    where s = size g

prop_HexHexGrid_distance_in_bounds ∷ HexHexGrid → Int → Int → Property
prop_HexHexGrid_distance_in_bounds g i j = nonEmpty g ==>
  distance g a b < 2*s
    where s = size g
          a = g `pointAt` i
          b = g `pointAt` j

-- If the ordering produced by hexHexGrid is ever changed, this property
-- may need to be changed too. It relies on the first and last elements being
-- on opposite edges.
prop_HexHexGrid_distance_edge_to_edge ∷ HexHexGrid → Property
prop_HexHexGrid_distance_edge_to_edge g = s > 0 ==> distance g a b ≡ 2*s - 2
  where ps = indices g
        a = head ps
        b = last ps
        s = size g

prop_HexHexGrid_neighbour_count_in_bounds ∷ HexHexGrid → Int → Property
prop_HexHexGrid_neighbour_count_in_bounds g i = nonEmpty g ==> 
  if tileCount g ≡ 1
    then length (neighbours g x) ≡ 0
    else length (neighbours g x) `elem` [2,3,4,5,6]
  where x = g `pointAt` i

prop_HexHexGrid_boundary_count_correct ∷ HexHexGrid → Property
prop_HexHexGrid_boundary_count_correct g = property $
  (length . boundary) g ≡ (f . size) g
  where f 0 = 0
        f 1 = 1
        f s = 6*(s-1)

prop_HexHexGrid_boundary_tiles_have_fewer_neighbours ∷ HexHexGrid → Property
prop_HexHexGrid_boundary_tiles_have_fewer_neighbours g = property $
  all (5>) . map (numNeighbours g) . boundary $ g


--
-- Parallelogrammatical hexagonal grids   
--

-- We want the number of tiles in a test grid to be ~ n
sizedParaHexGrid ∷ Int → Gen ParaHexGrid
sizedParaHexGrid n = do
  r ← choose (0,n)
  let c = n `div` (r+1)
  return $ paraHexGrid r c

instance Arbitrary ParaHexGrid where
  arbitrary = sized sizedParaHexGrid

prop_ParaHexGrid_tile_count_correct ∷ ParaHexGrid → Property
prop_ParaHexGrid_tile_count_correct g = property $ 
  tileCount g ≡ r*c
    where (r, c) = size g

prop_ParaHexGrid_distance_in_bounds ∷ ParaHexGrid → Int → Int → Property
prop_ParaHexGrid_distance_in_bounds g i j = nonEmpty g ==>
  property $ distance g a b ≤ r+c-2
    where (r, c) = size g
          a = g `pointAt` i
          b = g `pointAt` j

-- If the ordering produced by paraHexGrid is ever changed, this property
-- may need to be changed too. It relies on the first and last elements being
-- at opposite corners on the longer diagonal.
prop_ParaHexGrid_distance_corner_to_corner ∷ ParaHexGrid → Property
prop_ParaHexGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance g a b ≡ r+c-2
    where ps = indices g
          a = head ps
          b = last ps
          (r, c) = size g

prop_ParaHexGrid_neighbour_count_in_bounds ∷ ParaHexGrid → Int → Property
prop_ParaHexGrid_neighbour_count_in_bounds g i = nonEmpty g ==> f
  where x = g `pointAt` i
        neighbourCount = length (neighbours g x)
        (r, c) = size g
        f | tileCount g ≡ 1 = neighbourCount ≡ 0
          | r ≡ 1 || c ≡ 1  = neighbourCount `elem` [1,2]
          | otherwise       = neighbourCount `elem` [2,3,4,5,6]

prop_ParaHexGrid_boundary_count_correct ∷ ParaHexGrid → Property
prop_ParaHexGrid_boundary_count_correct g = property $
  (length . boundary) g ≡ (cartesianBoundaryCount . size) g

prop_ParaHexGrid_boundary_tiles_have_fewer_neighbours ∷ HexHexGrid → Property
prop_ParaHexGrid_boundary_tiles_have_fewer_neighbours g = property $
  all (5>) . map (numNeighbours g) . boundary $ g


test ∷ Test
test = testGroup "Math.Geometry.GridQC"
  [
    -- TriTriGrid tests
    testProperty "prop_TriTriGrid_tile_count_correct"
      prop_TriTriGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - TriTriGrid"
      (prop_distance_reflexive ∷ TriTriGrid → Int → Property),
    testProperty "prop_distance_symmetric - TriTriGrid"
      (prop_distance_symmetric ∷ TriTriGrid → Int → Int → Property),
    testProperty "prop_minDistance_cw_distance - TriTriGrid"
      (prop_minDistance_cw_distance ∷ TriTriGrid → Int → [Int] → Property),
    testProperty "prop_grid_and_boundary_are_both_null_or_not - TriTriGrid"
      (prop_grid_and_boundary_are_both_null_or_not ∷ TriTriGrid → Property),
    testProperty "prop_boundary_in_grid - TriTriGrid"
      (prop_boundary_in_grid ∷ TriTriGrid → Property),
    testProperty "prop_TriTriGrid_boundary_count_correct"
      prop_TriTriGrid_boundary_count_correct,
    testProperty "prop_TriTriGrid_boundary_tiles_have_fewer_neighbours"
      prop_TriTriGrid_boundary_tiles_have_fewer_neighbours,
    testProperty "prop_centres_equidistant_from_boundary - TriTriGrid"
      (prop_centres_equidistant_from_boundary ∷ TriTriGrid → Property),
    testProperty "prop_centres_farthest_from_boundary - TriTriGrid"
      (prop_centres_farthest_from_boundary ∷ TriTriGrid → Int → Property),
    testProperty "prop_TriTriGrid_distance_in_bounds"
      prop_TriTriGrid_distance_in_bounds,
    testProperty "prop_TriTriGrid_distance_edge_to_edge"
      prop_TriTriGrid_distance_edge_to_edge,
    testProperty "prop_TriTriGrid_neighbour_count_in_bounds"
      prop_TriTriGrid_neighbour_count_in_bounds,
    testProperty "prop_neighbours_cw_viewpoint - TriTriGrid"
      (prop_neighbours_cw_viewpoint ∷ TriTriGrid → Int → Property),
    testProperty "prop_edges_cw_neighbours - TriTriGrid"
      ( prop_edges_cw_neighbours ∷ TriTriGrid → Int → Property),
    testProperty "prop_edges_are_adjacent - TriTriGrid"
      ( prop_edges_are_adjacent ∷ TriTriGrid → Property),
    testProperty "prop_adjacentTilesToward_moves_closer - TriTriGrid"
      ( prop_adjacentTilesToward_moves_closer ∷ 
          TriTriGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_have_min_length - TriTriGrid"
      ( prop_minimal_paths_have_min_length ∷ 
          TriTriGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_are_valid - TriTriGrid"
      ( prop_minimal_paths_are_valid ∷ TriTriGrid → Int → Int → Property),

    -- ParaTriGrid tests
    testProperty "prop_ParaTriGrid_tile_count_correct"
      prop_ParaTriGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - ParaTriGrid"
      (prop_distance_reflexive ∷ ParaTriGrid → Int → Property),
    testProperty "prop_distance_symmetric - ParaTriGrid"
      (prop_distance_symmetric ∷ ParaTriGrid → Int → Int → Property),
    testProperty "prop_minDistance_cw_distance - ParaTriGrid"
      (prop_minDistance_cw_distance ∷ ParaTriGrid → Int → [Int] → Property),
    testProperty "prop_grid_and_boundary_are_both_null_or_not - ParaTriGrid"
      (prop_grid_and_boundary_are_both_null_or_not ∷ ParaTriGrid → Property),
    testProperty "prop_boundary_in_grid - ParaTriGrid"
      (prop_boundary_in_grid ∷ ParaTriGrid → Property),
    testProperty "prop_ParaTriGrid_boundary_count_correct"
      prop_ParaTriGrid_boundary_count_correct,
    testProperty "prop_ParaTriGrid_boundary_tiles_have_fewer_neighbours"
      prop_ParaTriGrid_boundary_tiles_have_fewer_neighbours,
    testProperty "prop_centres_equidistant_from_boundary - ParaTriGrid"
      (prop_centres_equidistant_from_boundary ∷ ParaTriGrid → Property),
    testProperty "prop_centres_farthest_from_boundary - ParaTriGrid"
      (prop_centres_farthest_from_boundary ∷ ParaTriGrid → Int → Property),
    testProperty "prop_ParaTriGrid_distance_in_bounds"
      prop_ParaTriGrid_distance_in_bounds,
    testProperty "prop_ParaTriGrid_distance_corner_to_corner"
      prop_ParaTriGrid_distance_corner_to_corner,
    testProperty "prop_ParaTriGrid_neighbour_count_in_bounds"
      prop_ParaTriGrid_neighbour_count_in_bounds,
    testProperty "prop_neighbours_cw_viewpoint - ParaTriGrid"
      (prop_neighbours_cw_viewpoint ∷ ParaTriGrid → Int → Property),
    testProperty "prop_edges_cw_neighbours - ParaTriGrid"
      ( prop_edges_cw_neighbours ∷ ParaTriGrid → Int → Property),
    testProperty "prop_edges_are_adjacent - ParaTriGrid"
      ( prop_edges_are_adjacent ∷ ParaTriGrid → Property),
    testProperty "prop_adjacentTilesToward_moves_closer - ParaTriGrid"
      ( prop_adjacentTilesToward_moves_closer ∷ 
          ParaTriGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_have_min_length - ParaTriGrid"
      ( prop_minimal_paths_have_min_length ∷ 
          ParaTriGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_are_valid - ParaTriGrid"
      ( prop_minimal_paths_are_valid ∷ ParaTriGrid → Int → Int → Property),

    -- RectSquareGrid tests
    testProperty "prop_RectSquareGrid_tile_count_correct"
      prop_RectSquareGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - RectSquareGrid"
      (prop_distance_reflexive ∷ RectSquareGrid → Int → Property),
    testProperty "prop_distance_symmetric - RectSquareGrid"
      (prop_distance_symmetric ∷ RectSquareGrid → Int → Int → Property),
    testProperty "prop_minDistance_cw_distance - RectSquareGrid"
      (prop_minDistance_cw_distance ∷ RectSquareGrid → Int → [Int] → Property),
    testProperty "prop_grid_and_boundary_are_both_null_or_not - RectSquareGrid"
      (prop_grid_and_boundary_are_both_null_or_not ∷ RectSquareGrid → Property),
    testProperty "prop_boundary_in_grid - RectSquareGrid"
      (prop_boundary_in_grid ∷ RectSquareGrid → Property),
    testProperty "prop_RectSquareGrid_boundary_count_correct"
      prop_RectSquareGrid_boundary_count_correct,
    testProperty "prop_RectSquareGrid_boundary_tiles_have_fewer_neighbours"
      prop_RectSquareGrid_boundary_tiles_have_fewer_neighbours,
    testProperty "prop_centres_equidistant_from_boundary - RectSquareGrid"
      (prop_centres_equidistant_from_boundary ∷ RectSquareGrid → Property),
    testProperty "prop_centres_farthest_from_boundary - RectSquareGrid"
      (prop_centres_farthest_from_boundary ∷ RectSquareGrid → Int → Property),
    testProperty "prop_RectSquareGrid_distance_in_bounds"
      prop_RectSquareGrid_distance_in_bounds,
    testProperty "prop_RectSquareGrid_distance_corner_to_corner"
      prop_RectSquareGrid_distance_corner_to_corner,
    testProperty "prop_RectSquareGrid_neighbour_count_in_bounds"
      prop_RectSquareGrid_neighbour_count_in_bounds,
    testProperty "prop_neighbours_cw_viewpoint - RectSquareGrid"
      (prop_neighbours_cw_viewpoint ∷ RectSquareGrid → Int → Property),
    testProperty "prop_edges_cw_neighbours - RectSquareGrid"
      ( prop_edges_cw_neighbours ∷ RectSquareGrid → Int → Property),
    testProperty "prop_edges_are_adjacent - RectSquareGrid"
      ( prop_edges_are_adjacent ∷ RectSquareGrid → Property),
    testProperty "prop_adjacentTilesToward_moves_closer - RectSquareGrid"
      ( prop_adjacentTilesToward_moves_closer ∷ 
          RectSquareGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_have_min_length - RectSquareGrid"
      ( prop_minimal_paths_have_min_length ∷ 
          RectSquareGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_are_valid - RectSquareGrid"
      ( prop_minimal_paths_are_valid ∷ RectSquareGrid → Int → Int → Property),
    testProperty "prop_RectSquareGrid_num_min_paths_correct"
      prop_RectSquareGrid_num_min_paths_correct,

    -- TorSquareGrid tests
    testProperty "prop_TorSquareGrid_tile_count_correct"
      prop_TorSquareGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - TorSquareGrid"
      (prop_distance_reflexive ∷ TorSquareGrid → Int → Property),
    testProperty "prop_distance_symmetric - TorSquareGrid"
      (prop_distance_symmetric ∷ TorSquareGrid → Int → Int → Property),
    testProperty "prop_minDistance_cw_distance - TorSquareGrid"
      (prop_minDistance_cw_distance ∷ TorSquareGrid → Int → [Int] → Property),
    testProperty "prop_TorSquareGrid_distance_in_bounds"
      prop_TorSquareGrid_distance_in_bounds,
    testProperty "prop_TorSquareGrid_distance_corner_to_corner"
      prop_TorSquareGrid_distance_corner_to_corner,
    testProperty "prop_TorSquareGrid_neighbour_count_in_bounds"
      prop_TorSquareGrid_neighbour_count_in_bounds,
    testProperty "prop_neighbours_cw_viewpoint - TorSquareGrid"
      (prop_neighbours_cw_viewpoint ∷ TorSquareGrid → Int → Property),
    testProperty "prop_edges_cw_neighbours - TorSquareGrid"
      ( prop_edges_cw_neighbours ∷ TorSquareGrid → Int → Property),
    testProperty "prop_edges_are_adjacent - TorSquareGrid"
      ( prop_edges_are_adjacent ∷ TorSquareGrid → Property),
    testProperty "prop_adjacentTilesToward_moves_closer - TorSquareGrid"
      ( prop_adjacentTilesToward_moves_closer ∷ 
          TorSquareGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_have_min_length - TorSquareGrid"
      ( prop_minimal_paths_have_min_length ∷ 
          TorSquareGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_are_valid - TorSquareGrid"
      ( prop_minimal_paths_are_valid ∷ TorSquareGrid → Int → Int → Property),

    -- HexHexGrid tests
    testProperty "prop_HexHexGrid_tile_count_correct"
      prop_HexHexGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - HexHexGrid"
      (prop_distance_reflexive ∷ HexHexGrid → Int → Property),
    testProperty "prop_distance_symmetric - HexHexGrid"
      (prop_distance_symmetric ∷ HexHexGrid → Int → Int → Property),
    testProperty "prop_minDistance_cw_distance - HexHexGrid"
      (prop_minDistance_cw_distance ∷ HexHexGrid → Int → [Int] → Property),
    testProperty "prop_grid_and_boundary_are_both_null_or_not - HexHexGrid"
      (prop_grid_and_boundary_are_both_null_or_not ∷ HexHexGrid → Property),
    testProperty "prop_boundary_in_grid - HexHexGrid"
      (prop_boundary_in_grid ∷ HexHexGrid → Property),
    testProperty "prop_HexHexGrid_boundary_count_correct"
      prop_HexHexGrid_boundary_count_correct,
    testProperty "prop_HexHexGrid_boundary_tiles_have_fewer_neighbours"
      prop_HexHexGrid_boundary_tiles_have_fewer_neighbours,
    testProperty "prop_centres_equidistant_from_boundary - HexHexGrid"
      (prop_centres_equidistant_from_boundary ∷ HexHexGrid → Property),
    testProperty "prop_centres_farthest_from_boundary - HexHexGrid"
      (prop_centres_farthest_from_boundary ∷ HexHexGrid → Int → Property),
    testProperty "prop_HexHexGrid_distance_in_bounds"
      prop_HexHexGrid_distance_in_bounds,
    testProperty "prop_HexHexGrid_distance_edge_to_edge"
      prop_HexHexGrid_distance_edge_to_edge,
    testProperty "prop_HexHexGrid_neighbour_count_in_bounds"
      prop_HexHexGrid_neighbour_count_in_bounds,
    testProperty "prop_neighbours_cw_viewpoint - HexHexGrid"
      (prop_neighbours_cw_viewpoint ∷ HexHexGrid → Int → Property),
    testProperty "prop_edges_cw_neighbours - HexHexGrid"
      ( prop_edges_cw_neighbours ∷ HexHexGrid → Int → Property),
    testProperty "prop_edges_are_adjacent - HexHexGrid"
      ( prop_edges_are_adjacent ∷ HexHexGrid → Property),
    testProperty "prop_adjacentTilesToward_moves_closer - HexHexGrid"
      ( prop_adjacentTilesToward_moves_closer ∷ 
          HexHexGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_have_min_length - HexHexGrid"
      ( prop_minimal_paths_have_min_length ∷ 
          HexHexGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_are_valid - HexHexGrid"
      ( prop_minimal_paths_are_valid ∷ HexHexGrid → Int → Int → Property),

    -- ParaHexGrid tests
    testProperty "prop_ParaHexGrid_tile_count_correct"
      prop_ParaHexGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - ParaHexGrid"
      (prop_distance_reflexive ∷ ParaHexGrid → Int → Property),
    testProperty "prop_distance_symmetric - ParaHexGrid"
      (prop_distance_symmetric ∷ ParaHexGrid → Int → Int → Property),
    testProperty "prop_minDistance_cw_distance - ParaHexGrid"
      (prop_minDistance_cw_distance ∷ ParaHexGrid → Int → [Int] → Property),
    testProperty "prop_grid_and_boundary_are_both_null_or_not - ParaHexGrid"
      (prop_grid_and_boundary_are_both_null_or_not ∷ ParaHexGrid → Property),
    testProperty "prop_boundary_in_grid - ParaHexGrid"
      (prop_boundary_in_grid ∷ ParaHexGrid → Property),
    testProperty "prop_ParaHexGrid_boundary_count_correct"
      prop_ParaHexGrid_boundary_count_correct,
    testProperty "prop_ParaHexGrid_boundary_tiles_have_fewer_neighbours"
      prop_ParaHexGrid_boundary_tiles_have_fewer_neighbours,
    testProperty "prop_centres_equidistant_from_boundary - ParaHexGrid"
      (prop_centres_equidistant_from_boundary ∷ ParaHexGrid → Property),
    testProperty "prop_centres_farthest_from_boundary - ParaHexGrid"
      (prop_centres_farthest_from_boundary ∷ ParaHexGrid → Int → Property),
    testProperty "prop_ParaHexGrid_distance_in_bounds"
      prop_ParaHexGrid_distance_in_bounds,
    testProperty "prop_ParaHexGrid_distance_corner_to_corner"
      prop_ParaHexGrid_distance_corner_to_corner,
    testProperty "prop_ParaHexGrid_neighbour_count_in_bounds"
      prop_ParaHexGrid_neighbour_count_in_bounds,
    testProperty "prop_neighbours_cw_viewpoint - ParaHexGrid"
      (prop_neighbours_cw_viewpoint ∷ ParaHexGrid → Int → Property),
    testProperty "prop_edges_cw_neighbours - ParaHexGrid"
      ( prop_edges_cw_neighbours ∷ ParaHexGrid → Int → Property),
    testProperty "prop_adjacentTilesToward_moves_closer - ParaHexGrid"
      ( prop_adjacentTilesToward_moves_closer ∷ 
          ParaHexGrid → Int → Int → Property),
    testProperty "prop_edges_are_adjacent - ParaHexGrid"
      ( prop_edges_are_adjacent ∷ ParaHexGrid → Property),
    testProperty "prop_minimal_paths_have_min_length - ParaHexGrid"
      ( prop_minimal_paths_have_min_length ∷ 
          ParaHexGrid → Int → Int → Property),
    testProperty "prop_minimal_paths_are_valid - ParaHexGrid"
      ( prop_minimal_paths_are_valid ∷ ParaHexGrid → Int → Int → Property)
 ]


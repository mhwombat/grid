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
pointIn ∷ Grid g s x ⇒ Int → g → x
pointIn i g = indices g !! (i `mod` n)
  where n = (length . indices) g

--
-- Tests that should apply to and are identical for all grids
--

prop_distance_reflexive ∷ Grid g s x ⇒ g → Int → Property
prop_distance_reflexive g i = nonEmpty g ==> distance a a g ≡ 0
  where a = i `pointIn` g

prop_distance_symmetric ∷ Grid g s x ⇒ g → Int → Int → Property
prop_distance_symmetric g i j = nonEmpty g ==> distance a b g ≡ distance b a g
  where a = i `pointIn` g
        b = j `pointIn` g

-- "cw" = "consistent with"

prop_neighbours_cw_viewpoint ∷ (Grid g s x, Ord x) ⇒ g → Int → Property
prop_neighbours_cw_viewpoint g i = n > 0 ==> 
  sort (a `neighbours` g) ≡ sort expected
    where n = (length . indices) g
          a = indices g !! (i `mod` n) -- make sure point is in grid
          expected = map fst $ filter (\p → 1 ≡ snd p) $ a `viewpoint` g

prop_edges_cw_neighbours ∷ (Grid g s x, Ord x) ⇒ g → Int → Property
prop_edges_cw_neighbours g i = n > 0 ==> 
  sort (a `neighbours` g) ≡ sort expected
    where n = (length . indices) g
          a = indices g !! (i `mod` n) -- make sure point is in grid
          nEdges = filter (`involves` a) $ edges g
          expected = filter (≠ a) $ nub $ map fst nEdges ++ map snd nEdges

involves ∷ Eq a ⇒ (a, a) → a → Bool
involves (a, b) c = c ≡ a || c ≡ b

prop_edges_are_adjacent ∷ (Grid g s x, Ord x) ⇒ g → Property
prop_edges_are_adjacent g = property $ and $ map f $ edges g
  where f (a, b) = isAdjacent a b g

isAdjacent :: Grid g s x => x -> x -> g -> Bool
isAdjacent a b g = (distance a b g) ≡ 1

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
  distance a b g ≤ 2*(s-1)
    where s = size g
          a = i `pointIn` g
          b = j `pointIn` g

-- If the ordering produced by triTriGrid is ever changed, this property
-- may need to be changed too. It relies on the first and last elements being
-- at corners.
prop_TriTriGrid_distance_edge_to_edge ∷ TriTriGrid → Property
prop_TriTriGrid_distance_edge_to_edge g = s > 0 ==> distance a b g ≡ 2*(s-1)
  where ps = indices g
        a = head ps
        b = last ps
        s = size g

prop_TriTriGrid_neighbour_count_in_bounds ∷ TriTriGrid → Int → Property
prop_TriTriGrid_neighbour_count_in_bounds g i = nonEmpty g ==>
  if tileCount g ≡ 1
    then length (x `neighbours` g) ≡ 0
    else length (x `neighbours` g) `elem` [1,2,3]
  where x = i `pointIn` g

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
  distance a b g ≤ 2*(r+c) - 3
    where (r, c) = size g
          a = i `pointIn` g
          b = j `pointIn` g

-- If the ordering produced by paraTriGrid is ever changed, this
-- property may need to be changed too. It relies on the first and last 
-- elements being at corners.
prop_ParaTriGrid_distance_corner_to_corner ∷ ParaTriGrid → Property
prop_ParaTriGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance a b g ≡ 2*(r+c) - 3
    where ps = indices g
          a = head ps
          b = last ps
          (r, c) = size g

prop_ParaTriGrid_neighbour_count_in_bounds ∷ ParaTriGrid → Int → Property
prop_ParaTriGrid_neighbour_count_in_bounds g i = nonEmpty g ==>
  if tileCount g ≡ 1
    then length (x `neighbours` g) ≡ 0
    else length (x `neighbours` g) `elem` [1,2,3]
  where x = i `pointIn` g

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
  distance a b g ≤ r + c - 2
    where (r, c) = size g
          a = i `pointIn` g
          b = j `pointIn` g

-- If the ordering produced by rectSquareGrid is ever changed, this
-- property may need to be changed too. It relies on the first and last 
-- elements being at opposite corners.
prop_RectSquareGrid_distance_corner_to_corner ∷ RectSquareGrid → Property
prop_RectSquareGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance a b g ≡ r + c - 2
    where (r, c) = size g
          ps = indices g
          a = head ps
          b = last ps

prop_RectSquareGrid_neighbour_count_in_bounds ∷ 
  RectSquareGrid → Int → Property
prop_RectSquareGrid_neighbour_count_in_bounds g i = nonEmpty g ==> f
  where x = i `pointIn` g
        neighbourCount = length (x `neighbours` g)
        (r, c) = size g
        f | tileCount g ≡ 1 = neighbourCount ≡ 0
          | r ≡ 1 || c ≡ 1  = neighbourCount `elem` [1,2]
          | otherwise       = neighbourCount `elem` [2,3,4]

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
  distance a b g ≤ (r+c) `div` 2
    where (r, c) = size g
          a = i `pointIn` g
          b = j `pointIn` g

-- If the ordering produced by torSquareGrid is ever changed, this property
-- may need to be changed too.
prop_TorSquareGrid_distance_corner_to_corner ∷ TorSquareGrid → Property
prop_TorSquareGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance a b g ≡ f
    where (r, c) = size g
          ps = indices g
          a = head ps
          b = last ps
          f | r ≡ 1 && c ≡ 1 = 0 -- zero-size torus
            | r ≡ 1 || c ≡ 1 = 1 -- a and b are the same
            | otherwise      = 2

prop_TorSquareGrid_neighbour_count_in_bounds ∷ TorSquareGrid → Int → Property
prop_TorSquareGrid_neighbour_count_in_bounds g i = nonEmpty g ==> f
  where x = i `pointIn` g
        neighbourCount = length (x `neighbours` g)
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
  distance a b g < 2*s
    where s = size g
          a = i `pointIn` g
          b = j `pointIn` g

-- If the ordering produced by hexHexGrid is ever changed, this property
-- may need to be changed too. It relies on the first and last elements being
-- on opposite edges.
prop_HexHexGrid_distance_edge_to_edge ∷ HexHexGrid → Property
prop_HexHexGrid_distance_edge_to_edge g = s > 0 ==> distance a b g ≡ 2*s - 2
  where ps = indices g
        a = head ps
        b = last ps
        s = size g

prop_HexHexGrid_neighbour_count_in_bounds ∷ HexHexGrid → Int → Property
prop_HexHexGrid_neighbour_count_in_bounds g i = nonEmpty g ==> 
  if tileCount g ≡ 1
    then length (x `neighbours` g) ≡ 0
    else length (x `neighbours` g) `elem` [2,3,4,5,6]
  where x = i `pointIn` g

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
  property $ distance a b g ≤ r+c-2
    where (r, c) = size g
          a = i `pointIn` g
          b = j `pointIn` g

-- If the ordering produced by paraHexGrid is ever changed, this property
-- may need to be changed too. It relies on the first and last elements being
-- at opposite corners on the longer diagonal.
prop_ParaHexGrid_distance_corner_to_corner ∷ ParaHexGrid → Property
prop_ParaHexGrid_distance_corner_to_corner g = r > 0 && c > 0 ==> 
  distance a b g ≡ r+c-2
    where ps = indices g
          a = head ps
          b = last ps
          (r, c) = size g

prop_ParaHexGrid_neighbour_count_in_bounds ∷ ParaHexGrid → Int → Property
prop_ParaHexGrid_neighbour_count_in_bounds g i = nonEmpty g ==> f
  where x = i `pointIn` g
        neighbourCount = length (x `neighbours` g)
        (r, c) = size g
        f | tileCount g ≡ 1 = neighbourCount ≡ 0
          | r ≡ 1 || c ≡ 1  = neighbourCount `elem` [1,2]
          | otherwise       = neighbourCount `elem` [2,3,4,5,6]

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
    -- ParaTriGrid tests
    testProperty "prop_ParaTriGrid_tile_count_correct"
      prop_ParaTriGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - ParaTriGrid"
      (prop_distance_reflexive ∷ ParaTriGrid → Int → Property),
    testProperty "prop_distance_symmetric - ParaTriGrid"
      (prop_distance_symmetric ∷ ParaTriGrid → Int → Int → Property),
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
    -- RectSquareGrid tests
    testProperty "prop_RectSquareGrid_tile_count_correct"
      prop_RectSquareGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - RectTriGrid"
      (prop_distance_reflexive ∷ RectSquareGrid → Int → Property),
    testProperty "prop_distance_symmetric - RectSquareGrid"
      (prop_distance_symmetric ∷ RectSquareGrid → Int → Int → Property),
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
    -- TorSquareGrid tests
    testProperty "prop_TorSquareGrid_tile_count_correct"
      prop_TorSquareGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - TorSquareGrid"
      (prop_distance_reflexive ∷ TorSquareGrid → Int → Property),
    testProperty "prop_distance_symmetric - TorSquareGrid"
      (prop_distance_symmetric ∷ TorSquareGrid → Int → Int → Property),
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
    -- HexHexGrid tests
    testProperty "prop_HexHexGrid_tile_count_correct"
      prop_HexHexGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - HexHexGrid"
      (prop_distance_reflexive ∷ HexHexGrid → Int → Property),
    testProperty "prop_distance_symmetric - HexHexGrid"
      (prop_distance_symmetric ∷ HexHexGrid → Int → Int → Property),
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
    -- ParaHexGrid tests
    testProperty "prop_ParaHexGrid_tile_count_correct"
      prop_ParaHexGrid_tile_count_correct,
    testProperty "prop_distance_reflexive - HexHexGrid"
      (prop_distance_reflexive ∷ ParaHexGrid → Int → Property),
    testProperty "prop_distance_symmetric - ParaHexGrid"
      (prop_distance_symmetric ∷ ParaHexGrid → Int → Int → Property),
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
    testProperty "prop_edges_are_adjacent - ParaHexGrid"
      ( prop_edges_are_adjacent ∷ ParaHexGrid → Property)
 ]


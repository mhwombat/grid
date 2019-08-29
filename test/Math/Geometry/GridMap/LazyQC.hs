------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.GridMap.LazyQC
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
    MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Geometry.GridMap.LazyQC
  (
    test
  ) where

import Data.List ((\\), foldl', intersect)
import Data.Maybe (isJust)
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy
import qualified Math.Geometry.Grid as G
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck 
  ((==>), Gen, Arbitrary, arbitrary, choose, Property, property,
    vectorOf, elements, sized)

-- We want the number of tiles in a test grid to be O(n)
sizedRectSquareGrid :: Int -> Gen RectSquareGrid
sizedRectSquareGrid n = do
  r <- choose (0,n)
  let c = n `div` (r+1)
  return $ rectSquareGrid r c

sizedGridMap :: Int -> Gen (LGridMap RectSquareGrid Int)
sizedGridMap n = do
  g <- sizedRectSquareGrid n
  vs <- vectorOf (G.tileCount g) arbitrary
  let gm = lazyGridMap g vs
  if G.null g
    then return gm
    else do
      -- Arbitrarily delete some values from the map
      m <- choose (0,n)
      ks <- sequence . replicate m . elements . G.indices $ g
      return $ foldl' (flip GM.delete) gm ks

instance Arbitrary (LGridMap RectSquareGrid Int) where
  arbitrary = sized sizedGridMap

-- data GridMapTD = GridMapTD (LGridMap RectSquareGrid Int) (Int,Int)
--   deriving (Eq, Show)

-- sizedGridMapTD :: Int -> Gen GridMapTD
-- sizedGridMapTD n = do
--   gm <- sizedGridMap (n+1)
--   k <- elements . indices $ gm
--   return $ GridMapTD gm k

-- instance Arbitrary GridMapTD where
--   arbitrary = sized sizedGridMapTD

selectIndex :: LGridMap RectSquareGrid Int -> Int -> (Int, Int)
selectIndex gm n = G.indices gm !! k
  where k = n `mod` G.tileCount gm

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf xs ys = Prelude.null (xs \\ ys)

mapValid :: LGridMap RectSquareGrid Int -> Bool
mapValid gm = keysWithValues `isSubsetOf` keys
  where keysWithValues = map fst . GM.toList $ gm
        keys = G.indices . GM.toGrid $ gm

prop_lookup_same_as_bang :: LGridMap RectSquareGrid Int -> Int -> Property
prop_lookup_same_as_bang gm n
  = G.nonNull gm && isJust (GM.lookup k gm) ==>
    Just (gm GM.! k) == GM.lookup k gm
  where k = selectIndex gm n

prop_insert_works :: LGridMap RectSquareGrid Int -> Int -> Int -> Property
prop_insert_works gm v n = G.nonNull gm ==> gm' GM.! k == v
  where k = selectIndex gm n
        gm' = GM.insert k v gm

prop_insert_never_alters_grid
  :: LGridMap RectSquareGrid Int -> (Int, Int) -> Int -> Property
prop_insert_never_alters_grid gm k v = property $
  GM.toGrid (GM.insert k v gm) == GM.toGrid gm

prop_insert_never_invalid
  :: LGridMap RectSquareGrid Int -> (Int, Int) -> Int -> Property
prop_insert_never_invalid gm k v = property $
  mapValid (GM.insert k v gm)

prop_delete_works :: LGridMap RectSquareGrid Int -> Int -> Property
prop_delete_works gm n = G.nonNull gm ==> GM.lookup k gm' == Nothing
  where k = selectIndex gm n
        gm' = GM.delete k gm

prop_delete_never_alters_grid
  :: LGridMap RectSquareGrid Int -> (Int, Int) -> Property
prop_delete_never_alters_grid gm k = property $
  GM.toGrid (GM.delete k gm) == GM.toGrid gm

prop_delete_never_invalid
  :: LGridMap RectSquareGrid Int -> (Int, Int) -> Property
prop_delete_never_invalid gm k = property $
  mapValid (GM.delete k gm)

prop_lazyGridMapIndexed_adds_all_valid_keys
  :: Int -> Int -> [(G.Index RectSquareGrid, Int)] -> Property
prop_lazyGridMapIndexed_adds_all_valid_keys n m kvs = property $
  (GM.keys gm) `intersect` (G.indices g) == GM.keys gm
  where gm = lazyGridMapIndexed g kvs
        g = rectSquareGrid n m

prop_lazyGridMapIndexed_never_adds_invalid_keys
  :: Int -> Int -> [(G.Index RectSquareGrid, Int)] -> Property
prop_lazyGridMapIndexed_never_adds_invalid_keys n m kvs = property $
  null ((GM.keys gm) \\ (G.indices g))
  where gm = lazyGridMapIndexed g kvs
        g = rectSquareGrid n m

test :: Test
test = testGroup "Math.Geometry.GridMap.LazyQC"
  [
    testProperty "prop_lookup_same_as_bang" prop_lookup_same_as_bang,
    testProperty "prop_insert_works" prop_insert_works,
    testProperty "prop_insert_never_alters_grid"
      prop_insert_never_alters_grid,
    testProperty "prop_insert_never_invalid" prop_insert_never_invalid,
    testProperty "prop_delete_works" prop_delete_works,
    testProperty "prop_delete_never_alters_grid"
      prop_delete_never_alters_grid,
    testProperty "prop_delete_never_invalid" prop_delete_never_invalid,
    testProperty "prop_lazyGridMapIndexed_adds_all_valid_keys"
      prop_lazyGridMapIndexed_adds_all_valid_keys,
    testProperty "prop_lazyGridMapIndexed_never_adds_invalid_keys"
      prop_lazyGridMapIndexed_never_adds_invalid_keys
  ]

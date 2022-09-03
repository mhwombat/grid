------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.GridMap.Lazy
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Ordered maps from tiles on a grid to values.
-- This module is a wrapper around @'Math.Geometry.Grid'@ and
-- @'Data.Map'@, in order to combine the functionality of grids and maps
-- into a single type.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Math.Geometry.GridMap.Lazy
  (
    LGridMap,
    lazyGridMap,
    lazyGridMapIndexed,
    empty
  ) where

import Prelude                    hiding (foldl, foldl1, foldr, foldr1, lookup,
                                   map, null)

import Data.Foldable              qualified as F (Foldable (..))
import Data.Map                   qualified as M
import Prelude                    qualified as P (map)
--import qualified Data.Map.Strict as Strict (Map)
import Data.Maybe                 (fromMaybe)
import GHC.Generics               (Generic)
import Math.Geometry.GridInternal qualified as G
import Math.Geometry.GridMap

-- | A map from tile positions in a grid to values.
data LGridMap g v =
  LGridMap { lgmGrid :: g, lgmMap :: M.Map (G.Index g) v }
    deriving Generic

-- | Construct a grid map which is strict in the keys (tile positions),
--   but lazy in the values.
lazyGridMap :: (Ord (G.Index g), G.Grid g) => g -> [v] -> LGridMap g v
lazyGridMap g vs = LGridMap g (M.fromList kvs)
  where kvs = zip ks vs
        ks = G.indices g

lazyGridMapIndexed :: (Ord (G.Index g), G.Grid g) => g -> [(G.Index g, v)] -> LGridMap g v
lazyGridMapIndexed g kvs = LGridMap g (M.fromList kvs')
  where kvs' = Prelude.filter (validIndex . fst) kvs
        validIndex k = g `G.contains` k

empty :: g -> LGridMap g v
empty g = LGridMap g M.empty

instance (G.Grid g, Ord (G.Index g)) => Functor (LGridMap g) where
  fmap f gm = lazyGridMap (lgmGrid gm) (P.map f vs)
    where vs = M.elems (lgmMap gm)

instance F.Foldable (LGridMap g) where
  fold = F.fold . lgmMap
  foldMap f g = F.foldMap f (lgmMap g)
  foldr f x g = F.foldr f x (lgmMap g)
  foldr' f x g = F.foldr' f x (lgmMap g)
  foldl f x g = F.foldl f x (lgmMap g)
  foldl' f x g = F.foldl' f x (lgmMap g)
--  foldr1 f x g = foldr1 f x (lgmMap g)
--  foldl1 f x g = foldl1 f x (lgmMap g)

instance G.Grid g => G.Grid (LGridMap g v) where
  type Index (LGridMap g v) = G.Index g
  type Direction (LGridMap g v) = G.Direction g
  indices = G.indices . lgmGrid
  distance g = G.distance (lgmGrid g)
  directionTo g = G.directionTo (lgmGrid g)
  neighbours g k = lgmGrid g `G.neighbours` k
  contains g k = lgmGrid g `G.contains` k
--  viewpoint g k = lgmGrid g `G.viewpoint` k
  tileCount  = G.tileCount . lgmGrid
  null = G.null . lgmGrid
  nonNull = G.nonNull . lgmGrid

instance G.FiniteGrid g => G.FiniteGrid (LGridMap g v) where
  type Size (LGridMap g v) = G.Size g
  size (LGridMap g _) = G.size g
  maxPossibleDistance (LGridMap g _) = G.maxPossibleDistance g

instance G.BoundedGrid g => G.BoundedGrid (LGridMap g v) where
  tileSideCount (LGridMap g _) = G.tileSideCount g

instance G.WrappedGrid g => G.WrappedGrid (LGridMap g v) where
  normalise (LGridMap g _) = G.normalise g
  denormalise (LGridMap g _) = G.denormalise g

instance (G.Grid g) => GridMap (LGridMap g) v where
  type BaseGrid (LGridMap g) v = g
  (!) gm k = toMap gm M.! k
  toMap = lgmMap
  toGrid = lgmGrid
  lookup k = M.lookup k . toMap
  insertWithKey f k v gm = if gm `G.contains` k
                   then gm { lgmMap = M.insertWithKey f k v $ lgmMap gm }
                   else gm
  delete k gm = if gm `G.contains` k
                   then gm { lgmMap = M.delete k $ lgmMap gm }
                   else gm
  adjustWithKey f k gm = gm { lgmMap = M.adjustWithKey f k (lgmMap gm)}
  alter f k gm = if gm `G.contains` k
                   then gm { lgmMap = M.alter f k $ lgmMap gm }
                   else gm
  findWithDefault v k = fromMaybe v . lookup k
  map f (LGridMap g m) = LGridMap g (M.map f m)
  mapWithKey f (LGridMap g m) = LGridMap g (M.mapWithKey f m)
  filter f (LGridMap g m) = LGridMap g (M.filter f m)
  filterWithKey f (LGridMap g m) = LGridMap g (M.filterWithKey f m)

deriving instance (Eq g, Eq v, Eq (G.Index g)) => Eq (LGridMap g v)

deriving instance (Show g, Show v, Show (G.Index g)) => Show (LGridMap g v)

deriving instance (Read g, Read v, Read (G.Index g), Ord (G.Index g)) => Read (LGridMap g v)

------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.HexGrid
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A regular arrangement of hexagonal tiles.
-- The userguide, with illustrations, is available at
-- <https://github.com/mhwombat/grid/wiki>.
-- Also see @Math.Geometry.Grid@ for examples of how to use this class.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Geometry.Grid.Hexagonal
  (
    -- * Unbounded grid with hexagonal tiles
    UnboundedHexGrid(..),
    -- * Hexagonal grid with hexagonal tiles
    HexHexGrid(..),
    hexHexGrid,
    -- * Parallelogram-shaped grid with hexagonal tiles
    ParaHexGrid(..),
    paraHexGrid
  ) where

import           Math.Geometry.Grid.HexagonalInternal

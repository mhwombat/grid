------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.HexGrid
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Same as @'Math.Geometry.Grid.Hexagonal'@, except the grids are
-- oriented so that the flat part of the hexagonal tiles is on the top.
-- The userguide, with illustrations, is available at
-- <https://github.com/mhwombat/grid/wiki>.
-- Also see @Math.Geometry.Grid@ for examples of how to use this class.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Geometry.Grid.Hexagonal2
  (
    -- * Unbounded grid with hexagonal tiles
    UnboundedHexGrid(..),
    -- * Hexagonal grid with hexagonal tiles
    HexHexGrid(..),
    hexHexGrid,
    -- * Rectangular grid with hexagonal tiles
    RectHexGrid(..),
    rectHexGrid
  ) where

import           Math.Geometry.Grid.HexagonalInternal2

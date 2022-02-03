------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.TriGrid
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A regular arrangement of triangular tiles.
-- The userguide, with illustrations, is available at
-- <https://github.com/mhwombat/grid/wiki>.
-- Also see @Math.Geometry.Grid@ for examples of how to use this class.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Geometry.Grid.Triangular
  (
    -- * Unbounded grid with triangular tiles
    UnboundedTriGrid(..),
    -- * Triangular grid with triangular tiles
    TriTriGrid(..),
    triTriGrid,
    -- * Parallelogram-shaped grid with triangular tiles
    ParaTriGrid(..),
    paraTriGrid,
    -- * Rectangular grid with triangular tiles
    RectTriGrid(..),
    rectTriGrid,
    -- * Toroidal grid with triangular tiles
    TorTriGrid(..),
    torTriGrid,
    -- * Cylindrical grids with triangular tiles
    YCylTriGrid(..),
    yCylTriGrid,
    XCylTriGrid(..),
    xCylTriGrid
  ) where

import           Math.Geometry.Grid.TriangularInternal

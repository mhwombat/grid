-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid
-- Copyright   :  (c) Amy de Buitléir 2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A regular arrangement of tiles. Grids have a variety of uses,
-- including games and self-organising maps.
--
-----------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, TypeSynonymInstances, 
  FlexibleInstances #-}

module Math.Geometry.Grid
  (
    -- * Generic
    Grid(..),
    -- * Grids with triangular tiles
    TriTriGrid,
    triTriGrid,
    ParaTriGrid,
    paraTriGrid,
    -- Grids with square tiles
    RectSquareGrid,
    rectSquareGrid,
    TorSquareGrid,
    torSquareGrid,
    -- * Grids with hexagonal tiles
    HexHexGrid,
    hexHexGrid,
    ParaHexGrid,
    paraHexGrid
  ) where

import Math.Geometry.GridInternal (Grid(..), TriTriGrid, triTriGrid, 
  ParaTriGrid, paraTriGrid, RectSquareGrid, rectSquareGrid, TorSquareGrid, 
  torSquareGrid, HexHexGrid, hexHexGrid, ParaHexGrid, paraHexGrid)

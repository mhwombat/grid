------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.OctGrid
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A regular arrangement of octagonal tiles.
-- Octagons won't tile a regular plane (there will be diamond-shaped
-- gaps between the tiles), but they will tile a /hyperbolic/ plane.
-- (Alternatively, you can think of these as squares on a board game
-- where diagonal moves are allowed.)
-- The userguide, with illustrations, is available at
-- <https://github.com/mhwombat/grid/wiki>.
-- Also see @Math.Geometry.Grid@ for examples of how to use this class.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Geometry.Grid.Octagonal
  (
    -- * Unbounded grid with octagonal tiles
    UnboundedOctGrid(..),
    -- * Rectangular grid with octagonal tiles
    RectOctGrid(..),
    rectOctGrid,
    -- * Toroidal grid with octagonal tiles
    TorOctGrid(..),
    torOctGrid
  ) where

import           Math.Geometry.Grid.OctagonalInternal

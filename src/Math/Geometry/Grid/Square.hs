------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.SquareGrid
-- Copyright   :  (c) Amy de Buitléir 2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A regular arrangement of square tiles.
-- The userguide, with illustrations, is available at 
-- <https://github.com/mhwombat/grid/wiki>.
-- Also see @Math.Geometry.Grid@ for examples of how to use this class.
--
------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, 
  FlexibleInstances #-}

module Math.Geometry.Grid.Square
  (
    -- * Unbounded grid with square tiles
    UnboundedSquareGrid,
    -- * Rectangular grid with square tiles
    RectSquareGrid,
    rectSquareGrid,
    -- * Toroidal grid with square tiles
    TorSquareGrid,
    torSquareGrid
  ) where

import Math.Geometry.Grid.SquareInternal (UnboundedSquareGrid, 
  RectSquareGrid, rectSquareGrid, TorSquareGrid, torSquareGrid)


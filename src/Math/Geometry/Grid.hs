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
-- The userguide is available at 
-- <https://github.com/mhwombat/grid/wiki>.
--
-- NOTE: Version 3.0 changed the order of parameters for many functions.
-- This makes it easier for the user to write mapping and folding
-- operations.
--
-----------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, TypeSynonymInstances, 
  FlexibleInstances #-}

module Math.Geometry.Grid
  (
    -- * The Grid class
    Grid(..),
    BoundedGrid(..),
    -- * Grids with triangular tiles
    TriTriGrid,
    triTriGrid,
    ParaTriGrid,
    paraTriGrid,
    -- * Grids with square tiles
    RectSquareGrid,
    rectSquareGrid,
    TorSquareGrid,
    torSquareGrid,
    -- * Grids with hexagonal tiles
    HexHexGrid,
    hexHexGrid,
    ParaHexGrid,
    paraHexGrid
    -- * Example
    -- $Example
  ) where

import Math.Geometry.GridInternal (Grid(..), BoundedGrid(..), 
  TriTriGrid, triTriGrid, ParaTriGrid, paraTriGrid, RectSquareGrid, 
  rectSquareGrid, TorSquareGrid, torSquareGrid, HexHexGrid, hexHexGrid, 
  ParaHexGrid, paraHexGrid)

{- $Example
   Create a grid.

>ghci> let g = hexHexGrid 3
>ghci> indices g
>[(-2,0),(-2,1),(-2,2),(-1,-1),(-1,0),(-1,1),(-1,2),(0,-2),(0,-1),(0,0),(0,1),(0,2),(1,-2),(1,-1),(1,0),(1,1),(2,-2),(2,-1),(2,0)]

   Find out the minimum number of moves to go from one tile in a grid to another
   tile, moving between adjacent tiles at each step.

>ghci> distance g (0,-2) (0,2)
>4

   Find out the minimum number of moves to go from one tile in a grid to any 
   other tile, moving between adjacent tiles at each step.

>ghci> viewpoint g (1,-2)
>[((-2,0),3),((-2,1),3),((-2,2),4),((-1,-1),2),((-1,0),2),((-1,1),3),((-1,2),4),((0,-2),1),((0,-1),1),((0,0),2),((0,1),3),((0,2),4),((1,-2),0),((1,-1),1),((1,0),2),((1,1),3),((2,-2),1),((2,-1),2),((2,0),3)]

   Find out which tiles are adjacent to a particular tile.

>ghci> neighbours g (-1,1)
>[(-2,1),(-2,2),(-1,2),(0,1),(0,0),(-1,0)]

   Find out if a tile is within the grid boundary.

>ghci> g `contains` (0,0)
>True
>ghci> g `contains` (0,12)
>False

   Find out the physical dimensions of the grid.

>ghci> size g
>3

   Find out the number of tiles in the grid.

>ghci> tileCount g
>19

   Check if a grid is empty (contains no tiles).

>ghci> empty g
>False
>ghci> nonEmpty g
>True

   Find all of the minimal paths between two points.

ghci> let g = hexHexGrid 3
ghci> minimalPaths g (0,0) (2,-1)
[[(0,0),(1,0),(2,-1)],[(0,0),(1,-1),(2,-1)]]

-}

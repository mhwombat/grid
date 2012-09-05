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
    -- * The Grid class
    Grid(..),
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

import Math.Geometry.GridInternal (Grid(..), TriTriGrid, triTriGrid, 
  ParaTriGrid, paraTriGrid, RectSquareGrid, rectSquareGrid, TorSquareGrid, 
  torSquareGrid, HexHexGrid, hexHexGrid, ParaHexGrid, paraHexGrid)

{- $Example
   Create a grid.

>ghci> let g = hexHexGrid 3
>ghci> indices g
>[(-2,0),(-2,1),(-2,2),(-1,-1),(-1,0),(-1,1),(-1,2),(0,-2),(0,-1),(0,0),(0,1),(0,2),(1,-2),(1,-1),(1,0),(1,1),(2,-2),(2,-1),(2,0)]

   Find out the minimum number of moves to go from one tile in a grid to another
   tile, moving between adjacent tiles at each step.

>ghci> distance (0,-2) (0,2) g
>4

   Find out the minimum number of moves to go from one tile in a grid to any 
   other tile, moving between adjacent tiles at each step.

>ghci> viewpoint (1,-2) g
>[((-2,0),3),((-2,1),3),((-2,2),4),((-1,-1),2),((-1,0),2),((-1,1),3),((-1,2),4),((0,-2),1),((0,-1),1),((0,0),2),((0,1),3),((0,2),4),((1,-2),0),((1,-1),1),((1,0),2),((1,1),3),((2,-2),1),((2,-1),2),((2,0),3)]

   Find out which tiles are adjacent to a particular tile.

>ghci> neighbours (-1,1) g
>[(-2,1),(-2,2),(-1,2),(0,1),(0,0),(-1,0)]

   Find out if a tile is within the grid boundary.

>ghci> inGrid (0,0) g
>True
>ghci> inGrid (0,12) g
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

-}

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
-- NOTE: Version 4.0 uses associated (type) synonyms instead of 
-- multi-parameter type classes.

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
    FiniteGrid(..),
    BoundedGrid(..),
    -- * Grids with triangular tiles
    UnboundedTriGrid,
    TriTriGrid,
    triTriGrid,
    ParaTriGrid,
    paraTriGrid,
    RectTriGrid,
    rectTriGrid,
    TorTriGrid,
    torTriGrid,
    -- * Grids with square tiles
    UnboundedSquareGrid,
    RectSquareGrid,
    rectSquareGrid,
    TorSquareGrid,
    torSquareGrid,
    -- * Grids with hexagonal tiles
    UnboundedHexGrid,
    HexHexGrid,
    hexHexGrid,
    ParaHexGrid,
    paraHexGrid
    -- * Example
    -- $Example
  ) where

import Math.Geometry.GridInternal (Grid(..), FiniteGrid(..), 
  BoundedGrid(..), UnboundedTriGrid, TriTriGrid, triTriGrid, 
  ParaTriGrid, paraTriGrid, RectTriGrid, rectTriGrid, 
  TorTriGrid, torTriGrid, UnboundedSquareGrid, 
  RectSquareGrid, rectSquareGrid, TorSquareGrid, torSquareGrid, 
  UnboundedHexGrid, HexHexGrid, hexHexGrid, ParaHexGrid, paraHexGrid)

{- $Example
   Create a grid.

>ghci> let g = hexHexGrid 3
>ghci> indices g
>[(-2,0),(-2,1),(-2,2),(-1,-1),(-1,0),(-1,1),(-1,2),(0,-2),(0,-1),(0,0),(0,1),(0,2),(1,-2),(1,-1),(1,0),(1,1),(2,-2),(2,-1),(2,0)]

   Find out if the specified index is contained within the grid.

>ghci> g `contains` (0,-2)
>True
>ghci> g `contains` (99,99)
>False

   Find out the minimum number of moves to go from one tile in a grid to
   another tile, moving between adjacent tiles at each step.

>ghci> distance g (0,-2) (0,2)
>4

   Find out the minimum number of moves to go from one tile in a grid to
   any other tile, moving between adjacent tiles at each step.

>ghci> viewpoint g (1,-2)
>[((-2,0),3),((-2,1),3),((-2,2),4),((-1,-1),2),((-1,0),2),((-1,1),3),((-1,2),4),((0,-2),1),((0,-1),1),((0,0),2),((0,1),3),((0,2),4),((1,-2),0),((1,-1),1),((1,0),2),((1,1),3),((2,-2),1),((2,-1),2),((2,0),3)]

   Find out which tiles are adjacent to a particular tile.

>ghci> neighbours g (-1,1)
>[(-2,1),(-2,2),(-1,2),(0,1),(0,0),(-1,0)]

   Find how many tiles are adjacent to a particular tile.
   (Note that the result is consistent with the result from the previous
   step.)

>ghci> numNeighbours g (-1,1)
>6

   Find out if an index is valid for the grid.

>ghci> g `contains` (0,0)
>True
>ghci> g `contains` (0,12)
>False

   Find out the physical dimensions of the grid.

>ghci> size g
>3

   Get the list of boundary tiles for a grid.

>ghci> boundary g
>[(-2,2),(-1,2),(0,2),(1,1),(2,0),(2,-1),(2,-2),(1,-2),(0,-2),(-1,-1),(-2,0),(-2,1)]

   Find out the number of tiles in the grid.

>ghci> tileCount g
>19

   Check if a grid is null (contains no tiles).

>ghci> null g
>False
>ghci> nonNull g
>True

   Find the central tile(s) (the tile(s) furthest from the boundary).

>ghci> centre g
>[(0,0)]

   Find all of the minimal paths between two points.

>ghci> let g = hexHexGrid 3
>ghci> minimalPaths g (0,0) (2,-1)
>[[(0,0),(1,0),(2,-1)],[(0,0),(1,-1),(2,-1)]]

   Find all of the pairs of tiles that are adjacent.

>ghci> edges g
>[((-2,0),(-2,1)),((-2,0),(-1,0)),((-2,0),(-1,-1)),((-2,1),(-2,2)),((-2,1),(-1,1)),((-2,1),(-1,0)),((-2,2),(-1,2)),((-2,2),(-1,1)),((-1,-1),(-1,0)),((-1,-1),(0,-1)),((-1,-1),(0,-2)),((-1,0),(-1,1)),((-1,0),(0,0)),((-1,0),(0,-1)),((-1,1),(-1,2)),((-1,1),(0,1)),((-1,1),(0,0)),((-1,2),(0,2)),((-1,2),(0,1)),((0,-2),(0,-1)),((0,-2),(1,-2)),((0,-1),(0,0)),((0,-1),(1,-1)),((0,-1),(1,-2)),((0,0),(0,1)),((0,0),(1,0)),((0,0),(1,-1)),((0,1),(0,2)),((0,1),(1,1)),((0,1),(1,0)),((0,2),(1,1)),((1,-2),(1,-1)),((1,-2),(2,-2)),((1,-1),(1,0)),((1,-1),(2,-1)),((1,-1),(2,-2)),((1,0),(1,1)),((1,0),(2,0)),((1,0),(2,-1)),((1,1),(2,0)),((2,-2),(2,-1)),((2,-1),(2,0))]

   Find out if two tiles are adjacent.

>ghci> isAdjacent g (-2,0) (-2,1)
>True
>ghci> isAdjacent g (-2,0) (0,1)
>False

-}


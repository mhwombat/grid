-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.Grid
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
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
-- In this package, tiles are called \"triangular\", \"square\", etc.,
-- based on the number of /neighbours/ they have.
-- For example, a square tile has four neighbours, and a hexagonal
-- tile has six.
-- There are only three regular polygons that can tile a plane:
-- triangles, squares, and hexagons.
-- Of course, other plane tilings are possible if you use irregular
-- polygons, or curved shapes, or if you combine tiles of different
-- shapes.
--
-- When you tile other surfaces, things get very interesting.
-- Octagons will tile a /hyperbolic/ plane.
-- (Alternatively, you can think of these as squares on a board game
-- where diagonal moves are allowed.)
--
-- For a board game, you probably want to choose the grid type based
-- on the number of /directions/ a player can move, rather than the
-- number of sides the tile will have when you display it.
-- For example, for a game that uses square tiles and allows the user
-- to move diagonally as well as horizontally and vertically,
-- consider using one of the grids with /octagonal/ tiles to model the
-- board.
-- You can still /display/ the tiles as squares, but for internal
-- calculations they are octagons.
--
-- NOTE: Version 6.0 moved the various grid flavours to sub-modules.
--
-- NOTE: Version 4.0 uses associated (type) synonyms instead of
-- multi-parameter type classes.
--
-- NOTE: Version 3.0 changed the order of parameters for many functions.
-- This makes it easier for the user to write mapping and folding
-- operations.
--
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Math.Geometry.Grid
  (
    -- * Example
    -- $Example

    -- * Grids
    Grid(indices, distance, minDistance, neighbours, neighbour,
      contains, tileCount, null, nonNull, edges, viewpoint,
      isAdjacent, adjacentTilesToward, minimalPaths, directionTo),
    Index,
    Direction,

    -- * Finite grids
    FiniteGrid(..),

    -- * Bounded grids
    BoundedGrid(..)
  ) where

import Math.Geometry.GridInternal (Grid(..), FiniteGrid(..),
  BoundedGrid(..))


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


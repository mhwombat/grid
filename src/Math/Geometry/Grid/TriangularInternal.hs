------------------------------------------------------------------------
-- |
-- Module      :  Math.Geometry.TriGridInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @TriGrid@ internals. Most developers
-- should use @TriGrid@ instead. This module is subject to change
-- without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Math.Geometry.Grid.TriangularInternal where

import           Prelude                    hiding (null)

import           Data.List                  (nub)
import           GHC.Generics               (Generic)
import           Math.Geometry.GridInternal

data TriDirection = South | Northwest | Northeast |
                      North | Southeast | Southwest
                        deriving (Read, Show, Eq, Generic)

-- | An unbounded grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data UnboundedTriGrid = UnboundedTriGrid deriving (Read, Show, Eq, Generic)

instance Grid UnboundedTriGrid where
  type Index UnboundedTriGrid = (Int, Int)
  type Direction UnboundedTriGrid = TriDirection
  indices _ = undefined
  neighbours _ (x,y) = if even y
                         then [(x-1,y+1), (x+1,y+1), (x+1,y-1)]
                         else [(x-1,y-1), (x-1,y+1), (x+1,y-1)]
  distance _ (x1, y1) (x2, y2) =
    maximum [abs (x2-x1), abs (y2-y1), abs(z2-z1)]
      where z1 = triZ x1 y1
            z2 = triZ x2 y2
  contains _ _ = True
  null _ = False
  nonNull _ = True
  directionTo _ (x1, y1) (x2, y2) =
    if even y1
      then f1 . f2 . f3 $ []
      else f4 . f5 . f6 $ []
    where f1 ds =  if y2 < y1 then South:ds else ds
          f2 ds =  if x2 < x1 then Northwest:ds else ds
          f3 ds =  if z2 < z1 then Northeast:ds else ds
          f4 ds =  if y2 > y1 then North:ds else ds
          f5 ds =  if x2 > x1 then Southeast:ds else ds
          f6 ds =  if z2 > z1 then Southwest:ds else ds
          z1 = triZ x1 y1
          z2 = triZ x2 y2


-- | For triangular tiles, it is convenient to define a third component
--   z.
triZ :: Int -> Int -> Int
triZ x y = if even y then -x - y else -x - y + 1

--
-- Triangular grids with triangular tiles
--

-- | A triangular grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TriTriGrid = TriTriGrid Int deriving (Read, Show, Eq, Generic)

instance Grid TriTriGrid where
  type Index TriTriGrid = (Int, Int)
  type Direction TriTriGrid = TriDirection
  indices (TriTriGrid s) = [(xx,yy) | xx <- [0..2*(s-1)],
                                      yy <- [0..2*(s-1)],
                                      (xx,yy) `inTriTriGrid` s]
  neighbours = neighboursBasedOn UnboundedTriGrid
  distance = distanceBasedOn UnboundedTriGrid
  contains (TriTriGrid s) (x, y) = inTriTriGrid (x,y) s
  directionTo = directionToBasedOn UnboundedTriGrid

inTriTriGrid :: (Int, Int) -> Int -> Bool
inTriTriGrid (x, y) s = x >= 0 && y >= 0 && even (x+y) && abs z <= 2*s-2
  where z = triZ x y

instance FiniteGrid TriTriGrid where
  type Size TriTriGrid = Int
  size (TriTriGrid s) = s
  maxPossibleDistance g@(TriTriGrid s) = distance g (0,0) (2*s-2,0)

instance BoundedGrid TriTriGrid where
  tileSideCount _ = 3
  boundary g = west ++ east ++ south
    where s = size g
          west = [(0,k) | k <- [0,2..2*s-2]]
          east = [(k,2*s-2-k) | k <- [2,4..2*s-2]]
          south = [(k,0) | k <- [2*s-4,2*s-6..2]]
  centre g = case s `mod` 3 of
    0 -> trefoilWithTop (k-1,k+1) where k = (2*s) `div` 3
    1 -> [(k,k)] where k = (2*(s-1)) `div` 3
    2 -> [(k+1,k+1)] where k = (2*(s-2)) `div` 3
    _ -> error "This will never happen."
    where s = size g
          trefoilWithTop (i,j) = [(i,j), (i+2, j-2), (i,j-2)]

{-# DEPRECATED triTriGrid "Use the TriTriGrid constructor instead." #-}

-- | @'triTriGrid' s@ returns a triangular grid with sides of
--   length @s@, using triangular tiles. If @s@ is nonnegative, the
--   resulting grid will have @s^2@ tiles. Otherwise, the resulting grid
--   will be null and the list of indices will be null.
triTriGrid :: Int -> TriTriGrid
triTriGrid = TriTriGrid

--
-- Parallelogrammatical grids with triangular tiles
--

-- | A Parallelogrammatical grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data ParaTriGrid = ParaTriGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid ParaTriGrid where
  type Index ParaTriGrid = (Int, Int)
  type Direction ParaTriGrid = TriDirection
  indices (ParaTriGrid (r, c)) = parallelogramIndices r c
  neighbours = neighboursBasedOn UnboundedTriGrid
  distance = distanceBasedOn UnboundedTriGrid
  directionTo = directionToBasedOn UnboundedTriGrid
  contains g (x,y) = 0 <= x && x < 2*c && 0 <= y && y < 2*r && even (x+y)
    where (r,c) = size g

instance FiniteGrid ParaTriGrid where
  type Size ParaTriGrid = (Int, Int)
  size (ParaTriGrid s) = s
  maxPossibleDistance g@(ParaTriGrid (r,c)) =
    distance g (0,0) (2*c-1,2*r-1)

instance BoundedGrid ParaTriGrid where
  tileSideCount _ = 3
  boundary g = west ++ north ++ east ++ south
    where (r,c) = size g
          west = [(0,k) | c>0, k <- [0,2..2*r-2]]
          north = [(k,2*r-1) | r>0, k <- [1,3..2*c-1]]
          east = [(2*c-1,k) | c>0, k <- [2*r-3,2*r-5..1]]
          south = [(k,0) | r>0, k <- [2*c-2,2*c-4..2]]
  centre g = f . size $ g
    where f (r,c)
            | odd r && odd c
                = [(c-1,r-1), (c,r)]
            | even r && even c && r == c
                = bowtie (c-1,r-1)
            | even r && even c && r > c
                = bowtie (c-1,r-3) ++ bowtie (c-1,r-1) ++ bowtie (c-1,r+1)
            | even r && even c && r < c
                = bowtie (c-3,r-1) ++ bowtie (c-1,r-1) ++ bowtie (c+1,r-1)
            | otherwise
                = [(c-1,r), (c,r-1)]
          bowtie (i,j) = [(i,j), (i+1,j+1)]

{-# DEPRECATED paraTriGrid "Use the ParaTriGrid constructor instead." #-}

-- | @'paraTriGrid' r c@ returns a grid in the shape of a
--   parallelogram with @r@ rows and @c@ columns, using triangular
--   tiles. If @r@ and @c@ are both nonnegative, the resulting grid will
--   have @2*r*c@ tiles. Otherwise, the resulting grid will be null and
--   the list of indices will be null.
paraTriGrid :: Int -> Int -> ParaTriGrid
paraTriGrid r c = ParaTriGrid (r, c)

parallelogramIndices :: Int -> Int -> [(Int, Int)]
parallelogramIndices r c =
  [(x,y) | x <- [0..2*c-1], y <- [0..2*r-1], even (x+y)]

--
-- Rectangular grids with triangular tiles
--

-- | A rectangular grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data RectTriGrid = RectTriGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid RectTriGrid where
  type Index RectTriGrid = (Int, Int)
  type Direction RectTriGrid = TriDirection
  indices (RectTriGrid (r, c))
    = [(x,y) | y <- [0..2*r-1], x <- [xMin y .. xMax c y], even (x+y)]
      where xMin y = if even y then w else w+1
              where w = -2*((y+1) `div` 4)
            xMax c2 y = xMin y + 2*(c2-1)
  neighbours = neighboursBasedOn UnboundedTriGrid
  distance = distanceBasedOn UnboundedTriGrid
  directionTo = directionToBasedOn UnboundedTriGrid
  -- TODO Implement faster "contains"

instance FiniteGrid RectTriGrid where
  type Size RectTriGrid = (Int, Int)
  size (RectTriGrid s) = s
  maxPossibleDistance g = -- TODO: make more efficient
    maximum . map (distance g (0,0)) . indices $ g

instance BoundedGrid RectTriGrid where
  tileSideCount _ = 3

{-# DEPRECATED rectTriGrid "Use the RectTriGrid constructor instead." #-}

-- | @'rectTriGrid' r c@ returns a grid in the shape of a
--   rectangle (with jagged edges) that has @r@ rows and @c@ columns,
--   using triangular tiles. If @r@ and @c@ are both nonnegative, the
--   resulting grid will have @2*r*c@ tiles. Otherwise, the resulting grid will be null and
--   the list of indices will be null.
rectTriGrid :: Int -> Int -> RectTriGrid
rectTriGrid r c = RectTriGrid (r,c)


--
-- Toroidal grids with triangular tiles
--

-- | A toroidal grid with triangular tiles.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data TorTriGrid = TorTriGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid TorTriGrid where
  type Index TorTriGrid = (Int, Int)
  type Direction TorTriGrid = TriDirection
  indices (TorTriGrid (r, c)) = parallelogramIndices r c
  neighbours = neighboursWrappedBasedOn UnboundedTriGrid
  neighbour = neighbourWrappedBasedOn UnboundedTriGrid
  distance = distanceWrappedBasedOn UnboundedTriGrid
  directionTo = directionToWrappedBasedOn UnboundedTriGrid
  isAdjacent g a b = distance g a b <= 1
  contains _ _ = True

instance FiniteGrid TorTriGrid where
  type Size TorTriGrid = (Int, Int)
  size (TorTriGrid s) = s
  maxPossibleDistance g = -- TODO: make more efficient
    maximum . map (distance g (0,0)) . indices $ g

instance WrappedGrid TorTriGrid where
  normalise g (x,y) | y < 0     = normalise g (x,y+2*r)
                    | y > 2*r-1 = normalise g (x,y-2*r)
                    | x < 0     = normalise g (x+2*c,y)
                    | x > 2*c-1 = normalise g (x-2*c,y)
                    | otherwise = (x,y)
    where (r, c) = size g
  denormalise g a = nub [ (x-2*c,y+2*r), (x,y+2*r), (x+2*c,y+2*r),
                          (x-2*c,y),     (x,y),     (x+2*c,y),
                          (x-2*c,y-2*r), (x,y-2*r), (x+2*c,y-2*r) ]
    where (r, c) = size g
          (x, y) = normalise g a

{-# DEPRECATED torTriGrid "Use the TorTriGrid constructor instead." #-}

-- | @'torTriGrid' r c@ returns a toroidal grid with @r@ rows and @c@
--   columns, using triangular tiles. The indexing method is the same as
--   for @ParaTriGrid@. If @r@ and @c@ are both nonnegative, the
--   resulting grid will have @2*r*c@ tiles. Otherwise, the resulting
--   grid will be null and the list of indices will be null.
torTriGrid :: Int -> Int -> TorTriGrid
torTriGrid r c = TorTriGrid (r,c)

--
-- Cylindrical grids with triangular tiles
--

-- | A cylindrical grid with triangular tiles, where the cylinder is
--   along the y-axis.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data YCylTriGrid = YCylTriGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid YCylTriGrid where
  type Index YCylTriGrid = (Int, Int)
  type Direction YCylTriGrid = TriDirection
  indices (YCylTriGrid (r, c)) = parallelogramIndices r c
  neighbours = neighboursWrappedBasedOn UnboundedTriGrid
  neighbour = neighbourWrappedBasedOn UnboundedTriGrid
  distance = distanceWrappedBasedOn UnboundedTriGrid
  directionTo = directionToWrappedBasedOn UnboundedTriGrid
  isAdjacent g a b = distance g a b <= 1
  contains g (x, y) = 0 <= y && y <= 2*r-1 && even (x+y)
    where (r, _) = size g

instance FiniteGrid YCylTriGrid where
  type Size YCylTriGrid = (Int, Int)
  size (YCylTriGrid s) = s
  maxPossibleDistance g = -- TODO: make more efficient
    maximum . map (distance g (0,0)) . indices $ g

instance WrappedGrid YCylTriGrid where
  normalise g (x,y) | x < 0     = normalise g (x+2*c,y)
                    | x > 2*c-1 = normalise g (x-2*c,y)
                    | otherwise = (x,y)
    where (_, c) = size g
  denormalise g a = nub [ (x-2*c,y), (x,y), (x+2*c,y) ]
    where (_, c) = size g
          (x, y) = normalise g a

{-# DEPRECATED yCylTriGrid "Use the YCylTriGrid constructor instead." #-}

-- | @'yCylTriGrid' r c@ returns a cylindrical grid with @r@ rows and
--   @c@ columns, using triangular tiles, where the cylinder is along
--   the y-axis. The indexing method is the same as for @ParaTriGrid@.
--   If @r@ and @c@ are both nonnegative, the resulting grid will have
--   @2*r*c@ tiles. Otherwise, the resulting grid will be null and the
--   list of indices will be null.
yCylTriGrid :: Int -> Int -> YCylTriGrid
yCylTriGrid r c = YCylTriGrid (r,c)

-- | A cylindrical grid with triangular tiles, where the cylinder is
--   along the x-axis.
--   The grid and its indexing scheme are illustrated in the user guide,
--   available at <https://github.com/mhwombat/grid/wiki>.
data XCylTriGrid = XCylTriGrid (Int, Int)
  deriving  (Read, Show, Eq, Generic)

instance Grid XCylTriGrid where
  type Index XCylTriGrid = (Int, Int)
  type Direction XCylTriGrid = TriDirection
  indices (XCylTriGrid (r, c)) = parallelogramIndices r c
  neighbours = neighboursWrappedBasedOn UnboundedTriGrid
  neighbour = neighbourWrappedBasedOn UnboundedTriGrid
  distance = distanceWrappedBasedOn UnboundedTriGrid
  directionTo = directionToWrappedBasedOn UnboundedTriGrid
  isAdjacent g a b = distance g a b <= 1
  contains g (x, y) = 0 <= x && x <= 2*c-1 && even (x+y)
    where (_, c) = size g

instance FiniteGrid XCylTriGrid where
  type Size XCylTriGrid = (Int, Int)
  size (XCylTriGrid s) = s
  maxPossibleDistance g = -- TODO: make more efficient
    maximum . map (distance g (0,0)) . indices $ g

instance WrappedGrid XCylTriGrid where
  normalise g (x,y) | y < 0     = normalise g (x,y+2*r)
                    | y > 2*r-1 = normalise g (x,y-2*r)
                    | otherwise = (x,y)
    where (r, _) = size g
  denormalise g a = nub [ (x,y-2*r), (x,y), (x,y+2*r) ]
    where (r, _) = size g
          (x, y) = normalise g a

{-# DEPRECATED xCylTriGrid "Use the XCylTriGrid constructor instead." #-}

-- | @'xCylTriGrid' r c@ returns a cylindrical grid with @r@ rows and
--   @c@ columns, using triangular tiles, where the cylinder is along
--   the y-axis. The indexing method is the same as for @ParaTriGrid@.
--   If @r@ and @c@ are both nonnegative, the resulting grid will have
--   @2*r*c@ tiles. Otherwise, the resulting grid will be null and the
--   list of indices will be null.
xCylTriGrid :: Int -> Int -> XCylTriGrid
xCylTriGrid r c = XCylTriGrid (r,c)


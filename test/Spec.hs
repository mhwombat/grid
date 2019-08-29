------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
import Math.Geometry.Grid.TriangularQC ( test )
import Math.Geometry.Grid.SquareQC ( test )
import Math.Geometry.Grid.HexagonalQC ( test )
import Math.Geometry.Grid.Hexagonal2QC ( test )
import Math.Geometry.Grid.OctagonalQC ( test )
import Math.Geometry.GridMap.LazyQC (test)

import Test.Framework ( defaultMain, Test )

tests :: [Test]
tests = 
  [ 
    Math.Geometry.Grid.TriangularQC.test,
    Math.Geometry.Grid.SquareQC.test,
    Math.Geometry.Grid.HexagonalQC.test,
    Math.Geometry.Grid.Hexagonal2QC.test,
    Math.Geometry.Grid.OctagonalQC.test,
    Math.Geometry.GridMap.LazyQC.test
  ]

main :: IO ()
main = defaultMain tests

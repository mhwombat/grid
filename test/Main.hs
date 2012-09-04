{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Math.Geometry.GridQC ( test )

import Test.Framework as TF ( defaultMain, Test )

tests ∷ [TF.Test]
tests = 
  [ 
    Math.Geometry.GridQC.test
  ]

main ∷ IO ()
main = defaultMain tests

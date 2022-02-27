{-# OPTIONS_GHC -Wall #-}

module HydrothermalVenture.HydrothermalVenture
  ( pointsWithAtLeastTwoOverlaps,
    VentLine,
  )
where

type VentLine = (Int, Int, Int, Int)

pointsWithAtLeastTwoOverlaps :: [VentLine] -> Int
pointsWithAtLeastTwoOverlaps _ = 0

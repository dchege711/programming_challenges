{-# OPTIONS_GHC -Wall #-}

module HydrothermalVenture.HydrothermalVenture
  ( pointsWithAtLeastTwoOverlaps,
    LineSegment (..),
  )
where

-- The input text is of the form:
--
-- 0,9 -> 5,9
-- 8,0 -> 0,8
--
-- The points don't make sense individually, so grouping them together as a line
-- segment makes sense.
data LineSegment = LineSegment {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int}

pointsWithAtLeastTwoOverlaps :: [LineSegment] -> Int
pointsWithAtLeastTwoOverlaps _ = 0

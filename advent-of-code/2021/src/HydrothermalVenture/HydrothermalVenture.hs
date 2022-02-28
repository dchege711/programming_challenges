{-# OPTIONS_GHC -Wall #-}

module HydrothermalVenture.HydrothermalVenture
  ( pointsWithAtLeastTwoOverlaps,
    LineSegment (..),
    Point (..)
  )
where

-- The input text is of the form:
--
-- 0,9 -> 5,9
-- 8,0 -> 0,8
--
-- The points don't make sense individually, so grouping them together as a line
-- segment makes sense.
data Point = Point {x :: Int, y :: Int}
data LineSegment = LineSegment {p1 :: Point, p2 :: Point}

pointsWithAtLeastTwoOverlaps :: [LineSegment] -> Int
pointsWithAtLeastTwoOverlaps _ = 0

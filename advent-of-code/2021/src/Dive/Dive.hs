{-# OPTIONS_GHC -Wall #-}

module Dive.Dive (productOfFinalPosition) where

-- One consideration in Haskell is the third-party library that I should use,
-- given that standard GHC seems quite lean. [1] should help in picking up
-- libraries that have lots of usage. For example, [2] helped me pick [3], which
-- uses Posix regex, and is fast, native, stable and lazy.
--
-- [1]: https://wiki.haskell.org/Category:Libraries
-- [2]: https://wiki.haskell.org/Regular_expressions
-- [3]: https://hackage.haskell.org/package/regex-tdfa

import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA

-- [1] suggests that I should be fine without deriving from the `Enum` data
-- type. Deriving from `Enum` helps me when I care about the mapping to an
-- underlying type, e.g. Int. [2]
--
-- [1]: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/5-tokenizer-data-types#enumerated-data-types
-- [2]: https://stackoverflow.com/questions/6000511/better-way-to-define-an-enum-in-haskell/6000520
-- [3]: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types
data DiveDirection = Up | Down | Forward deriving (Eq, Show)

diveDirection :: String -> Maybe DiveDirection
diveDirection "forward" = Just Forward
diveDirection "up" = Just Up
diveDirection "down" = Just Down
diveDirection _ = Nothing

parseSubMatches :: [String] -> Maybe (DiveDirection, Int)
parseSubMatches [dir, mag] =
  let direction = diveDirection dir
      magnitude = readMaybe mag :: Maybe Int
      parsedDirAndMag =
        if isJust direction && isJust magnitude
          then Just (fromJust direction, fromJust magnitude)
          else Nothing
   in parsedDirAndMag
parseSubMatches _ = Nothing

-- Surprised that defining the constant as `STEP_REGEX` does not compile:
--
--    Not in scope: data constructor ‘FORWARD’typecheck
--
-- Variable names must start with a lowercase letter, and anything that is
-- uppercase is interpreted as a Data Constructor. [1]
--
-- [1]: https://stackoverflow.com/a/28381111/7812406
stepRegex :: [Char]
stepRegex = "([a-z]+) ([0-9]+)" -- regex-tdfa doesn't have \d for digits

directionAndMagnitude :: String -> Maybe (DiveDirection, Int)
directionAndMagnitude s =
  -- The format is (beforeMatch, firstMatch, afterMatch, [subMatches])
  let (_, _, _, subMatches) = s =~ stepRegex :: (String, String, String, [String])
  -- Haskell lists are ordinary single-linked lists. Except operations on the
  -- first element (e.g. prepend, get, remove), the rest of the operations
  -- (including getting the length and indexing) are linear-time.
  --
  -- [1]: https://wiki.haskell.org/How_to_work_on_lists
   in parseSubMatches subMatches

applySign :: (DiveDirection, Int) -> Int
applySign (Up, i) = -i
applySign (Down, i) = i
applySign (Forward, i) = i


productOfFinalPosition :: [String] -> Int
productOfFinalPosition steps =
  -- In general, use `where` for locally-defined functions, and `let` for
  -- intermediate values. [1]
  --
  -- I don't fully get the nuance in [2], but it might come in handy later as
  -- I get more experience with Haskell. A brush stroke would be `let` places
  -- fewer restrictions, and `where` is more readable, but `where` may obscure
  -- inefficient code that redefines local functions.
  --
  -- [1]: http://www.cse.unsw.edu.au/~cs3161/14s2/StyleGuide.html#sec-5-1-1
  -- [2]: https://wiki.haskell.org/Let_vs._Where
  let -- I'm already familiar with `total = sum xs`. However, how do I compute
      -- `horizontalPos` and `verticalPos` while iterating through `steps` once?
      parsedSteps = mapMaybe directionAndMagnitude steps

      -- `fst` is a utility function for the first member of a pair.
      -- https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples
      finalHorizontalPos = sum $ map snd $ filter (\x -> fst x == Forward) parsedSteps
      finalVerticalPos = sum $ map applySign $ filter (\x -> fst x == Up || fst x == Down) parsedSteps
   in finalHorizontalPos * finalVerticalPos

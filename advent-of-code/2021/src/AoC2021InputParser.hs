{-# OPTIONS_GHC -Wall #-}

-- {-# LANGUAGE FlexibleContexts #-}

-- The `(..)` syntax represents all of the constructors for the data type. [1]
-- Without that export, we can pattern-match in BinaryDiagnostic.hs because we
-- run into a "Not in scope: data constructor ‘BinaryDiagnostics’" error.
--
-- [1]: https://stackoverflow.com/a/34548070/7812406
module AoC2021InputParser (parseBinaryDiagnosticInput, parseBingoInput) where

import BinaryDiagnostic.BinaryDiagnostic (BinaryDiagnostics(..), diagNums, diagWidth)
import Control.DeepSeq (($!!))
import Data.String (IsString (fromString))
import GiantSquid.GiantSquid (DrawnNumbers, Tile, Board)
import Paths_advent_of_code_y2021 (getDataFileName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Data.Maybe (listToMaybe, isJust)
import Data.Char (digitToInt)
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)
import Text.Read (readMaybe)

-- The `Numeric` module has a `readBin` function [1], but for some reason, I get
-- a "Variable not in scope: readBin" error. However, `readDec`, `readOct` and
-- `readHex` work...
--
-- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Numeric.html#v:readBin
readBin' :: String -> Int
readBin' binString = fst $ foldr f (0, 1) binString
  where
    f c (s, powerOf2) = (s + powerOf2 * digitToInt c, powerOf2 * 2)

-- The file is a list of binary digits of the same width, e.g.
--
-- 00100
-- 11110
parseBinaryDiagnosticInput :: FilePath -> IO BinaryDiagnostics
parseBinaryDiagnosticInput fp = do
  validatedFP <- getDataFileName fp
  withFile
    validatedFP
    ReadMode
    ( \h -> do
        s <- hGetContents h
        let ls = lines (fromString s)
        let width = maybe 0 length (listToMaybe ls)
        -- With lazy I/O, `h` gets closed as soon as we leave `withFile`. This
        -- introduces errors of the form "hGetContents: illegal operation
        -- (delayed read on closed handle)". [1]
        --
        -- In the expression `f $!! x`, `x` is fully evaluated before `f` is
        -- applied to it. [1] [2]
        --
        -- Alternatively, one can use `readFile` which holds the file open until
        -- it has finished reading the file [1]. However, [3] says that
        -- `readFile` reads the file lazily. Hmm...
        --
        -- [1]: https://stackoverflow.com/a/26949379/7812406
        -- [2]: https://hackage.haskell.org/package/deepseq-1.4.6.1/docs/Control-DeepSeq.html#v:-36--33--33-
        -- [3]: https://hackage.haskell.org/package/base-4.16.0.0/docs/html#v:readFile
        return $!! (BinaryDiagnostics{diagWidth=width, diagNums=map readBin' ls})
    )

-- `endBy` expects the very last item to be followed by the separator. It
-- continues parsing until it can't parse any more content. [1]
--
-- TODO: Is it possible to define `bingoFile` as `endBy bingoSection doubleEOL`?
-- The last double EOL is partly consumed by `bingoElementSeparator` that also
-- matches a new line.
--
-- [1]: http://book.realworldhaskell.org/read/using-parsec.html
bingoFile = endBy bingoSection endOfLine

-- `sepBy` takes two functions as arguments. The first function parses some sort
-- of content, while the second function parses a separator. `sepBy` starts by
-- trying to parse content, then separators, and alternates back and forth until
-- it can't parse a separator. It returns a list of all the content it was able
-- to parse. [1]
--
-- [1]: http://book.realworldhaskell.org/read/using-parsec.html
bingoSection = sepBy bingoElement bingoElementSeparator
bingoElement = many digit
bingoElementSeparator = try (char ',') <|> try (char ' ') <?> "separator for element"

-- `try` applies a parser, and if it fails, then `try` behaves as if it hadn't
-- consumed any input at all, and tries the option on the right of the `<|>`.
-- Note that `try` only has an effect if its on the left side of a `<|>`. [1]
--
-- `<?>` tries the parser on its left. In the event of a failure, it presents an
-- error message instead of trying another parser. The error message should
-- complete the sentence, "Expecting...". [1]
--
-- [1]: http://book.realworldhaskell.org/read/using-parsec.html
-- eol = try (string "\n\r\n\r")
--         <|> try (string "\r\n\r\n")
--         <|> try (string "\n\n")
--         <|> string "\r\r"
--         <?> "end of line followed by empty line"

-- File format: the first line contains the numbers to draw. The rest is a new
-- line followed by a 5x5 grid of numbers representing a board.
--
-- 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
--
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19
--
--  3 15  0  2 22
--  9 18 13 17  5
-- 19  8  7 25 23
-- 20 11 10 24  4
-- 14 21 16 12  6
parseBingoInput :: FilePath -> IO (DrawnNumbers, [Board])
parseBingoInput fp = do
  dataFp <- getDataFileName fp
  -- Objective: Try using `readFile` and see if `($!!)` is needed to fully
  -- evaluate the contents before exiting this function.
  fileContents <- readFile dataFp
  case parse bingoFile "" fileContents of
    Left e  -> do putStrLn "Error parsing input"
                  print e
                  return ([], [])
    Right r -> do let parseInt :: String -> Int
                      parseInt s = read s :: Int

                      drawnNumbers = map parseInt (head r)

                      tile :: String -> Tile
                      tile x = (parseInt x, False)

                      -- TODO: Figure out how to parse multiple spaces as
                      -- separators, and get rid of the `isValidNum` filter.
                      isValidNum :: String -> Bool
                      isValidNum s = isJust (readMaybe s :: Maybe Int)

                      parseBoards :: [[String]] -> [Board]
                      parseBoards ([_]:l1:l2:l3:l4:l5:ls) =
                        let
                          nums = V.filter isValidNum (V.fromList(l1 ++ l2 ++ l3 ++ l4 ++ l5))
                          board = V.map tile nums
                        in (board, False) : parseBoards ls
                      parseBoards [l1, l2, l3, l4, l5] =
                        let
                          nums = V.filter isValidNum (V.fromList(l1 ++ l2 ++ l3 ++ l4 ++ l5))
                          board = V.map tile nums
                        in [(board, False)]
                      parseBoards _ = []

                  return (drawnNumbers, parseBoards (tail r))

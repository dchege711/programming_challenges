{-# OPTIONS_GHC -Wall #-}

module AoC2021InputParser (parseBinaryDiagnosticInput) where

-- import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString (fromString))
import Paths_advent_of_code_y2021 (getDataFileName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

-- The `Numeric` module has a `readBin` function [1], but for some reason, I get
-- a "Variable not in scope: readBin" error. However, `readDec`, `readOct` and
-- `readHex` work...
--
-- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Numeric.html#v:readBin
readBin' :: String -> Int
readBin' binString = fst $ foldr f (0, 1) binString
  where
    f c (s, powerOf2) =
      let cDigit = if c == '0' then 0 else 1
       in (s + powerOf2 * cDigit, powerOf2 * 2)

-- The file is a list of binary digits of the same width, e.g.
--
-- 00100
-- 11110
parseBinaryDiagnosticInput :: FilePath -> IO [Int]
parseBinaryDiagnosticInput fp = do
  validatedFP <- getDataFileName fp
  withFile
    validatedFP
    ReadMode
    ( \h -> do
        s <- hGetContents h
        let ls = lines (fromString s)
        return (map readBin' ls)
    )

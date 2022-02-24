{-# OPTIONS_GHC -Wall #-}

-- The `(..)` syntax represents all of the constructors for the data type. [1]
-- Without that export, we can pattern-match in BinaryDiagnostic.hs because we
-- run into a "Not in scope: data constructor ‘BinaryDiagnostics’" error.
--
-- [1]: https://stackoverflow.com/a/34548070/7812406
module AoC2021InputParser (BinaryDiagnostics(..), parseBinaryDiagnosticInput) where

-- import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString (fromString))
import Paths_advent_of_code_y2021 (getDataFileName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Data.Maybe (listToMaybe)
import Data.Char (digitToInt)

-- The `Numeric` module has a `readBin` function [1], but for some reason, I get
-- a "Variable not in scope: readBin" error. However, `readDec`, `readOct` and
-- `readHex` work...
--
-- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Numeric.html#v:readBin
readBin' :: String -> Int
readBin' binString = fst $ foldr f (0, 1) binString
  where
    f c (s, powerOf2) = (s + powerOf2 * digitToInt c, powerOf2 * 2)

-- Without the underscore prefix, I need to add `diagWidth` and `diagNums` to the
-- export list to avoid `Wunused-top-binds` [1]. The field names share the top
-- level namespace with ordinary variables and classes. [2] That's kinda
-- inconvenient.
--
-- [1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html?highlight=unused-top-binds#ghc-flag--Wunused-top-binds
-- [2]: https://www.haskell.org/tutorial/moretypes.html#sect6.2
data BinaryDiagnostics = BinaryDiagnostics { diagWidth :: Int, diagNums :: [Int]}

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
        return (BinaryDiagnostics{diagWidth=width, diagNums=map readBin' ls})
    )

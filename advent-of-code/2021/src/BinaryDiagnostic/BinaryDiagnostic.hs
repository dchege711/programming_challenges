{-# OPTIONS_GHC -Wall #-}
{-# Language RecordWildCards #-}

module BinaryDiagnostic.BinaryDiagnostic (powerConsumption) where

import AoC2021InputParser (BinaryDiagnostics(..), diagNums, diagWidth)
import Data.Bits (Bits(testBit))

-- | `toBitList n b` returns a `[Int]` representing the b-least significant
-- | bits of `n`, e.g. `toBitList 22 5 == [1, 0, 1, 1, 0]`.
toBitList :: Int -> Int -> [Int]
toBitList n numBits = map (\i -> if testBit n i then 1 else 0) [0 .. (numBits-1)]

-- | `fromBitList ds` returns the `Int` formed when `ds` is treated like a bit
-- | representation of an intege, e.g. `fromBitList [1, 0, 1, 1, 0] == 22`.
fromBitList :: [Int] -> Int
fromBitList ds = fst $ foldr f (0, 1) ds
    where
        f d (s, powerOf2) = (s + powerOf2 * d, powerOf2 * 2)

-- The BinaryLiterals language extension adds syntactic sugar for `0b11001001`
-- [1]. But I didn't end up using binary representation. `Data.Bits` provides
-- utilities for working with `Int`, so why not? [2]
--
-- [1]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/binary_literals.html
-- [2]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Bits.html


powerConsumption :: BinaryDiagnostics -> Int
powerConsumption BinaryDiagnostics{ .. } =
    let updateCumulativeFrequencies :: Int -> [Int] -> [Int]
        updateCumulativeFrequencies num cumulativeBitFrequencies =
            zipWith (+) cumulativeBitFrequencies (toBitList num diagWidth)

        frequenciesToZeroOne :: [Int] -> [Int]
        frequenciesToZeroOne = map (\num -> if num <= 0 then 0 else 1)

        -- Calculate the bit frequencies. The result is an `[Int]` of length
        -- `diagWidth`. If an element is negative, then the most common bit at
        -- that index is `0`. We assume no ties in frequency.
        majorityBits = frequenciesToZeroOne $ foldr updateCumulativeFrequencies (replicate diagWidth 0) diagNums

        gammaRate = fromBitList majorityBits

        flipZerosAndOnes :: [Int] -> [Int]
        flipZerosAndOnes = map (\num -> if num == 0 then 1 else 0)

        epsilonRate = fromBitList (flipZerosAndOnes majorityBits)

    in gammaRate * epsilonRate

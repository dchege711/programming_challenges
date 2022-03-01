module Main (main) where

import AoC2021InputParser (parseBinaryDiagnosticInput, parseBingoInput, parseHydrothermalVents)
import BinaryDiagnostic.BinaryDiagnostic (lifeSupportRating, powerConsumption)
import Data.String (IsString (fromString))
import Dive.Dive (productOfFinalPosition, productOfFinalPositionWithNewIntepretation)
import GiantSquid.GiantSquid (scoreOfFirstWinningBoard, scoreOfLastWinningBoard)
import HydrothermalVenture.HydrothermalVenture
  ( pointsWithAtLeastTwoRightSegmentOverlaps,
    pointsWithAtLeastTwoSegmentOverlaps,
  )
import Paths_advent_of_code_y2021 (getDataFileName)
import SonarSweep ( num3MeasurementIncreases, numIncreases )
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Test.HUnit (Counts, Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)

testSonarSweep :: Test
testSonarSweep =
  TestCase
    ( do
        fp <- getDataFileName "src/scratchpad/01-sonar-sweep.sample.txt"
        withFile
          fp
          ReadMode
          ( \h -> do
              s <- hGetContents h
              let ls = lines (fromString s)
              assertEqual "numIncreases," 7 (SonarSweep.numIncreases ls)
              assertEqual "numIncreases," 5 (SonarSweep.num3MeasurementIncreases ls)
          )
    )

testDive :: Test
testDive =
  TestCase
    ( do
        fp <- getDataFileName "src/Dive/scratchpad/sample.txt"
        withFile
          fp
          ReadMode
          ( \h -> do
              s <- hGetContents h
              let ls = lines (fromString s)
              assertEqual "Stale Interpretation," 150 (productOfFinalPosition ls)
              assertEqual
                "Correct Interpretation,"
                900
                (productOfFinalPositionWithNewIntepretation ls)
          )
    )

testBinaryDiagnostic :: Test
testBinaryDiagnostic =
  TestCase
    ( do
        input <- parseBinaryDiagnosticInput "src/BinaryDiagnostic/scratchpad/sample.txt"
        assertEqual "Power Consumption," 198 (powerConsumption input)
        assertEqual "Life Support Rating," 230 (lifeSupportRating input)
    )

testGiantSquid :: Test
testGiantSquid =
  TestCase
    ( do
        input <- parseBingoInput "src/GiantSquid/scratchpad/sample.txt"
        assertEqual
          "Score of First Winning Board,"
          4512
          (scoreOfFirstWinningBoard input)
        assertEqual
          "Score of Last Winning Board,"
          1924
          (scoreOfLastWinningBoard input)
    )

testHydrothermalVenture :: Test
testHydrothermalVenture =
  TestCase
    ( do
        input <- parseHydrothermalVents "src/HydrothermalVenture/scratchpad/sample.txt"
        assertEqual
          "Num Points w/ >= 2 Right Segments Overlapping,"
          5
          (pointsWithAtLeastTwoRightSegmentOverlaps input)
        assertEqual
          "Num Points w/ >= 2 Segments Overlapping,"
          12
          (pointsWithAtLeastTwoSegmentOverlaps input)
    )

tests :: Test
tests =
  TestList
    [ TestLabel "Day 01: Sonar Sweep" testSonarSweep,
      TestLabel "Day 02: Dive!" testDive,
      TestLabel "Day 03: Binary Diagnostic" testBinaryDiagnostic,
      TestLabel "Day 04: Giant Squid" testGiantSquid,
      TestLabel "Day 05: Hydrothermal Venture" testHydrothermalVenture
    ]

main :: IO Counts
main = do
  runTestTT tests

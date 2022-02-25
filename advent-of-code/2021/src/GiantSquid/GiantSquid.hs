module GiantSquid.GiantSquid (DrawnNumbers, Tile, Board, scoreOfWinningBoard) where

import Data.Vector as V

-- File format: the first line contains the numbers to draw. The rest is a new
-- line followed by a 5x5 grid of numbers representing a board.
--
-- 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
--
-- 22 13 17 11  0 8  2 23  4 24 21  9 14 16  7 6 10  3 18  5 1 12 20 15 19
--
--  3 15  0  2 22 9 18 13 17  5 19  8  7 25 23 20 11 10 24  4 14 21 16 12  6
--
-- The numbers to draw can be represented as an `[Int]`.
--
-- A board can be represented as a flat list-like of `(Int, Bool)` tuples, with
-- helper functions accessing the cell at `(i, j)`. Haskell types are immutable,
-- so updating a board involves changing the entire board. I'll need to do a lot
-- of indexing. [1] evaluates trade-offs for various containers. Candidates:
-- `List`, `Data.Sequence`, and `Data.Vector.*`.
--
-- Typical operations will be updating a matching tile (if immutable, then
-- creating new 25-element list), and then checking if the board wins (at most
-- 10 lookups). The need for `fmap` and indexing makes me choose `Data.Vector`
-- for the `Board` type.
--
-- Does changing a board in a list of `Board`s recreate the whole vector?
-- Seems like it would because I'd be using `map`. Presumably though, only one
-- `[Board]` will be created because all `Boards` will be processed first.
--
-- [1]: https://www.curiosities.dev/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/03-binary-diagnostic/#efficiency-of-collections-types
-- [2]: https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector-Primitive.html#t:Prim

type DrawnNumbers = [Int]
type Tile = (Int, Bool)
type Board = V.Vector Tile

scoreOfWinningBoard :: Int
scoreOfWinningBoard = 50

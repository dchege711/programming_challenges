{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module GiantSquid.GiantSquid
  ( DrawnNumbers,
    Tile,
    Board,
    scoreOfFirstWinningBoard,
    scoreOfLastWinningBoard,
  )
where

import qualified Data.Vector as V
import Data.Maybe (fromJust)

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

type Tiles = V.Vector Tile

type Board = (Tiles, Bool)

boardWinsFromIndex :: Tiles -> Int -> Bool
boardWinsFromIndex tiles idx =
  -- TODO: Add error handling? This function crashes with idx = 25
  let columnTiles = [idx - 5, idx - 10 .. 0] ++ [idx] ++ [idx + 5, idx + 10 .. 24]
      mod5 = idx `mod` 5
      rowTiles = [idx - mod5, idx - mod5 + 1 .. idx + 5 - mod5 - 1]

      -- | Return `True` iff the tiles in the indices are all marked.
      bingo :: [Int] -> Bool
      bingo (j : js) = let tileIsMarked = snd (tiles V.! j)
                       in if null js
                           then tileIsMarked
                           else tileIsMarked && bingo js
      bingo []       = False

   in bingo columnTiles || bingo rowTiles

type TileWithIndex = (Tile, Int)

indicesWithMatch :: Tiles -> Int -> V.Vector Int
indicesWithMatch tiles num =
  let tileMatches :: Int -> TileWithIndex -> Bool
      tileMatches numToMatch (tile, _) = fst tile == numToMatch

      tilesWithIndex :: Tiles -> V.Vector TileWithIndex
      -- Unxpected infinite loop.
      --
      -- > l1 = [1, 2, 3]
      -- > l2 = [1 .. ]
      -- > zip l1 l2              -- [(1,1), (2,2), (3,3)]
      -- > v1 = [1, 2, 3]
      -- > v2 = [1 .. ]
      -- > V.zip v1 v2            -- Infinite loop!
      tilesWithIndex t = V.zip t (V.fromList [0 .. (length t)])

      matchingTilesWithIndex :: V.Vector TileWithIndex
      matchingTilesWithIndex = V.filter (tileMatches num) (tilesWithIndex tiles)
   in V.map snd matchingTilesWithIndex

playRoundOnBoard :: Int -> Board -> Board
playRoundOnBoard num board =
  let updateTile :: Int -> Tile -> Tile
      updateTile n tile = if fst tile == n then (n, True) else tile

      updatedTiles = V.map (updateTile num) (fst board)
      matchedIndices = V.toList (indicesWithMatch updatedTiles num)

      boardWins :: Tiles -> [Int] -> Bool
      boardWins _ [] = False
      boardWins tiles [i] = boardWinsFromIndex tiles i
      boardWins tiles (i : is) = boardWinsFromIndex tiles i || boardWins tiles is
   in (updatedTiles, boardWins updatedTiles matchedIndices)

type BoardWithLastCalledNum = (Board, Int)

playBingo :: DrawnNumbers -> [Board] -> BoardWithLastCalledNum
playBingo (n : nums) currBoards =
  let boardsAfterRound = map (playRoundOnBoard n) currBoards
      winner = filter snd boardsAfterRound
   in -- What is the idiomatic way of saying `if true`?
      if null winner then playBingo nums boardsAfterRound else (head winner, n)
playBingo [] currBoards = (head currBoards, 0)

scoreOfBoard :: BoardWithLastCalledNum -> Int
scoreOfBoard (board, lastCalledNum) =
  let unmarkedTiles :: Tiles
      unmarkedTiles = V.filter (not . snd) (fst board)
   in sum (V.map fst unmarkedTiles) * lastCalledNum

scoreOfFirstWinningBoard :: (DrawnNumbers, [Board]) -> Int
scoreOfFirstWinningBoard (drawnNums, boards) =
  scoreOfBoard (playBingo drawnNums boards)

playLastBoardBingo :: DrawnNumbers -> [Board] -> [BoardWithLastCalledNum] -> Maybe BoardWithLastCalledNum

playLastBoardBingo [n] boards winningBoards =
    let boardsAfterRound = map (playRoundOnBoard n) boards
        currWinners = reverse (filter snd boardsAfterRound)
        lastWinningBoardWithNum = if null currWinners
            then head winningBoards
            else (head currWinners, n)
    in Just lastWinningBoardWithNum

playLastBoardBingo (n:ns) boards winningBoards =
    let boardsAfterRound = map (playRoundOnBoard n) boards
        currWinners = reverse (filter snd boardsAfterRound)
        pendingBoards = filter (not . snd) boardsAfterRound
        allWinners = map (\b -> (b, n)) currWinners ++ winningBoards
    in playLastBoardBingo ns pendingBoards allWinners

playLastBoardBingo _ _ _ = Nothing

scoreOfLastWinningBoard :: (DrawnNumbers, [Board]) -> Int
scoreOfLastWinningBoard (drawnNums, boards) =
    -- TODO: error handling?
    scoreOfBoard(fromJust $ playLastBoardBingo drawnNums boards [])

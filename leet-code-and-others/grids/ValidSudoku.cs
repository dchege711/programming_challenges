using System.Collections.Specialized;

public class Solution {
    private static readonly int BOARD_LENGTH = 9;
    private static readonly int NUM_SUBBOXES_PER_LENGTH = 3;
    private static readonly int SUBBOX_LENGTH = BOARD_LENGTH / NUM_SUBBOXES_PER_LENGTH;
    private static readonly int VALID_CHECK_BIT_MASK = 1 << 0;

    public bool IsValidSudoku(char[][] board) {
        if (board.Length != BOARD_LENGTH || board[0].Length != BOARD_LENGTH)
            throw new ArgumentException($"Board is not a square of length {BOARD_LENGTH}");

        int?[,] sudokuBoard = new int?[BOARD_LENGTH, BOARD_LENGTH];
        for (int r = 0; r < BOARD_LENGTH; r++)
        {
            for (int c = 0; c < BOARD_LENGTH; c++)
            {
                int x = board[r][c] - '0';
                sudokuBoard[r, c] = (x >= 1 && x <= 9) ? x : null;
            }
        }

        // Validate columns
        for (int c = 0; c < BOARD_LENGTH; c++)
        {
            BitVector32 seenValues = new BitVector32(0);
            for (int r = 0; r < BOARD_LENGTH; r++)
            {
                seenValues = CheckIfTileIsValid(r, c, seenValues);
                if (!seenValues[VALID_CHECK_BIT_MASK])
                    return false;
            }
        }

        // Validate rows
        for (int r = 0; r < BOARD_LENGTH; r++)
        {
            BitVector32 seenValues = new BitVector32(0);
            for (int c = 0; c < BOARD_LENGTH; c++)
            {
                seenValues = CheckIfTileIsValid(r, c, seenValues);
                if (!seenValues[VALID_CHECK_BIT_MASK])
                    return false;
            }
        }

        // Validate 3x3 sub-boxes
        for (int br = 0; br < NUM_SUBBOXES_PER_LENGTH; br++)
        {
            for (int bc = 0; bc < NUM_SUBBOXES_PER_LENGTH; bc++)
            {
                BitVector32 seenValues = new BitVector32(0);
                for (int r = br * SUBBOX_LENGTH; r < (br + 1) * SUBBOX_LENGTH; r++)
                {
                    for (int c = bc * SUBBOX_LENGTH; c < (bc + 1) * SUBBOX_LENGTH; c++)
                    {
                        seenValues = CheckIfTileIsValid(r, c, seenValues);
                        if (!seenValues[VALID_CHECK_BIT_MASK])
                            return false;
                    }
                }
            }
        }

        return true;

        BitVector32 CheckIfTileIsValid(int r, int c, BitVector32 seenValues)
        {
            if (sudokuBoard[r, c] is int x)
            {
                int mask = 1 << x;
                if (seenValues[mask])
                {
                    seenValues[VALID_CHECK_BIT_MASK] = false;
                    return seenValues;
                }
                seenValues[mask] = true;
            }
            seenValues[VALID_CHECK_BIT_MASK] = true;
            return seenValues;
        }
    }
}

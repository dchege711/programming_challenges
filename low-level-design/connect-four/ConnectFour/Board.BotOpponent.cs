namespace ConnectFour;

public partial class Board
{
    public Board()
    {}

    public Board(Board other)
    {
        for (int r = 0; r < RowCount; r++)
        {
            for (int c = 0; c < ColCount; c++)
            {
                _grid[r, c] = other._grid[r, c];
            }
        }
    }

    public int GetColCount() => ColCount;
}

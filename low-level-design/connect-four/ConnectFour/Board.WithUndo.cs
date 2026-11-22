namespace ConnectFour;

public partial class Board
{
    public void Clear(Coordinate coord)
    {
        if (!InBounds(coord))
            throw new InvalidOperationException($"{coord} is not on the game board.");

        var isTopMost = coord.R == RowCount - 1 || _grid[coord.R + 1, coord.C] == null;
        if (!isTopMost)
            throw new InvalidOperationException($"A disc sits atop {coord}.");

        _grid[coord.R, coord.C] = null;
    }
}

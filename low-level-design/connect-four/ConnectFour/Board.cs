namespace ConnectFour;

public partial class Board
{
    public bool CanPlaceDisc(int column) =>
        column >= 0 && column < ColCount && _grid[RowCount - 1, column] == null;

    public bool IsFull() => Enumerable.Range(0, ColCount).All(c => _grid[0, c] != null);

    public Coordinate PlaceDisc(DiscColor color, int column)
    {
        if (!CanPlaceDisc(column))
            throw new InvalidOperationException($"Cannot drop into {column}");

        int row = Enumerable.Range(0, RowCount).First(r => _grid[r, column] == null);
        _grid[row, column] = color;
        return new(row, column);
    }

    public bool CheckWin(Coordinate coord)
    {
        if (!InBounds(coord))
            throw new InvalidOperationException($"{coord} is not on the game board.");

        return WinningDeltas.Any(delta => StreakInEitherDirection(coord, delta) >= StreakLengthForWin);
    }

    private bool InBounds(Coordinate coord) =>
        coord.R >= 0 && coord.R < RowCount && coord.C >= 0 && coord.C < ColCount;

    private int StreakInEitherDirection(Coordinate origin, Delta delta) =>
        StreakInDirection(origin, delta) + StreakInDirection(origin, delta * -1) + 1;

    private int StreakInDirection(Coordinate origin, Delta delta)
    {
        if (!InBounds(origin))
            throw new InvalidOperationException($"{origin} is not a valid starting point.");

        var targetColor = _grid[origin.R, origin.C];
        var streakLength = 0;

        for (int i = 1; i < StreakLengthForWin; i++)
        {
            var coord = origin + (delta * i);
            if (!InBounds(coord) || _grid[coord.R, coord.C] != targetColor)
                break;

            streakLength += 1;
        }

        return streakLength;
    }

    private const int RowCount = 6;
    private const int ColCount = 7;
    private const int StreakLengthForWin = 4;
    private readonly IReadOnlyList<Delta> WinningDeltas =
        [new Delta(0, 1), new Delta(1, 0), new Delta(1, 1)];

    private readonly DiscColor?[,] _grid = new DiscColor?[RowCount, ColCount];
}

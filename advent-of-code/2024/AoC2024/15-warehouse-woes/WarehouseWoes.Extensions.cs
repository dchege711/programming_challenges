using System.Diagnostics;
using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public static class WarehouseWoesExtensions
{
    extension(Coordinate source)
    {
        public static Coordinate operator +(Coordinate coord, Delta delta) =>
            new(coord.R + delta.dR, coord.C + delta.dC);

        public static Coordinate operator -(Coordinate coord, Delta delta) =>
            new(coord.R - delta.dR, coord.C - delta.dC);
    }

    public static bool IsInBounds(this CellType[,] grid, Coordinate coordinate) =>
        coordinate.R >= 0 && coordinate.R < grid.GetLength(0)
        && coordinate.C >= 0 && coordinate.C < grid.GetLength(1);

    public static Delta ToDelta(this Direction direction) => direction switch
    {
        Direction.Down => new(1, 0),
        Direction.Up => new(-1, 0),
        Direction.Left => new(0, -1),
        Direction.Right => new(0, 1),
        _ => throw ExhaustiveMatch.Failed(direction)
    };

    public static bool IsLateral(this Direction direction) =>
        direction == Direction.Left || direction == Direction.Right;

    public static int ToGpsCoordinate(this Coordinate coordinate) =>
        (coordinate.R * 100) + coordinate.C;

    public static int SumBoxGpsCoordinates(this CellType[,] grid) =>
        Enumerable.Range(0, grid.GetLength(0))
            .SelectMany(r => Enumerable.Range(0, grid.GetLength(1))
                .Select(c => new Coordinate(r, c)))
            .Where(coordinate => grid[coordinate.R, coordinate.C] is CellType.Box or CellType.BoxStart)
            .Sum(ToGpsCoordinate);

    public static void Visualize(this CellType[,] grid, Coordinate robotPosition)
    {
        for (int r = 0; r < grid.GetLength(0); r++)
        {
            for (int c = 0; c < grid.GetLength(1); c++)
            {
                if (r == robotPosition.R && c == robotPosition.C)
                {
                    Debug.Write('@');
                    continue;
                }

                var val = grid[r, c] switch
                {
                    CellType.Wall => '#',
                    CellType.Box => 'O',
                    CellType.Free => '.',
                    CellType.BoxStart => '[',
                    CellType.BoxEnd => ']',
                    _ => throw ExhaustiveMatch.Failed(grid[r, c])
                };
                Debug.Write(val);
            }
            Debug.Write('\n');
        }
        Debug.WriteLine("");
    }
}

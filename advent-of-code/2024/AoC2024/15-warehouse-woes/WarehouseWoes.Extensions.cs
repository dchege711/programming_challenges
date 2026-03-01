using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public static class WarehouseWoesExtensions
{
    public static Coordinate Move(this Coordinate coordinate, Delta delta) =>
        new(coordinate.R + delta.dR, coordinate.C + delta.dC);
    
    public static bool IsInBounds(this Coordinate coordinate, CellType[,] grid) =>
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

    public static Delta Reverse(this Delta delta) =>
        new(delta.dR * -1, delta.dC * -1);

    public static int ToGpsCoordinate(this Coordinate coordinate) =>
        (coordinate.R * 100) + coordinate.C;
}

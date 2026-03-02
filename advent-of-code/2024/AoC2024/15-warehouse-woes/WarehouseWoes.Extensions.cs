using System.Diagnostics;
using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public static class WarehouseWoesExtensions
{
    public static Coordinate Move(this Coordinate coordinate, Delta delta) =>
        new(coordinate.R + delta.dR, coordinate.C + delta.dC);

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

    public static bool IsLateral(this Direction direction) =>
        direction == Direction.Left || direction == Direction.Right;

    public static int ToGpsCoordinate(this Coordinate coordinate) =>
        (coordinate.R * 100) + coordinate.C;

    public static bool IsInBounds(this Grid grid, Coordinate coordinate) =>
        coordinate.R >= 0 && coordinate.R < grid.RowCount
        && coordinate.C >= 0 && coordinate.C < grid.ColCount;

    public static CellType GetCellType(this Grid grid, Coordinate coord)
    {
        if (grid.Walls.Contains(coord))
            return CellType.Wall;
        else if (grid.Boxes.Contains(coord))
            return grid.BoxWidth == 1 ? CellType.Box : CellType.BoxStart;
        else if (grid.BoxWidth == 2 && grid.Boxes.Contains(coord.Move(new(0, -1))))
            return CellType.BoxEnd;
        else
            return CellType.Free;
    }

    public static bool IsFree(this Grid grid, Coordinate coord) =>
        !grid.HasWallOrBox(coord);

    public static bool IsWall(this Grid grid, Coordinate coord) =>
        grid.Walls.Contains(coord);

    public static bool HasWallOrBox(this Grid grid, Coordinate coord) =>
        grid.Walls.Contains(coord)
        || grid.Boxes.Contains(coord)
        || (grid.BoxWidth == 2 && grid.Boxes.Contains(coord.Move(new(0, -1))));

    public static void Visualize(this Grid grid)
    {
        for (int r = 0; r < grid.RowCount; r++)
        {
            for (int c = 0; c < grid.ColCount; c++)
            {
                Coordinate coord = new(r, c);
                if (coord == grid.RobotPosition)
                {
                    Debug.Write('@');
                    continue;
                }

                var cellType = grid.GetCellType(coord);
                var val = cellType switch
                {
                    CellType.Wall => '#',
                    CellType.Free => '.',
                    CellType.Box => 'O',
                    CellType.BoxStart => '[',
                    CellType.BoxEnd => ']',
                    _ => throw ExhaustiveMatch.Failed(cellType)
                };
                Debug.Write(val);
            }
            Debug.Write('\n');
        }
        Debug.WriteLine("");
    }
}

using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static int PartOne(string filePath)
    {
        var (grid, robotPosition) = ParseGrid(filePath);

        foreach (var direction in ParseMoves(filePath))
            robotPosition = MoveInGrid(grid, robotPosition, direction);

        return SumBoxGpsCoordinates(grid);
    }

    private static Coordinate MoveInGrid(CellType[,] grid, Coordinate origin, Direction direction)
    {
        var delta = direction.ToDelta();
        var next = origin.Move(delta);

        if (!next.IsInBounds(grid))
            return origin;
        
        return grid[next.R, next.C] switch
        {
            CellType.Wall => origin,
            CellType.Free => next,
            CellType.Box => PushBoxes(grid, origin, delta),
            _ => throw ExhaustiveMatch.Failed(grid[next.R, next.C])
        };
    }

    private static Coordinate PushBoxes(CellType[,] grid, Coordinate origin, Delta delta)
    {
        Coordinate? maybeFreeSpot = null;
        Coordinate next = origin.Move(delta);
        while (next.IsInBounds(grid))
        {
            var cellType = grid[next.R, next.C];

            if (cellType == CellType.Wall)
                break;

            if (cellType == CellType.Free)
            {
                maybeFreeSpot = next;
                break;
            }
            
            next = next.Move(delta);
        }

        if (maybeFreeSpot is not Coordinate freeSpot)
            return origin;
        
        Delta reverseDelta = new(delta.dR * -1, delta.dC * -1);
        while (freeSpot != origin)
        {
            var previous = freeSpot.Move(reverseDelta);
            grid[freeSpot.R, freeSpot.C] = grid[previous.R, previous.C];
            freeSpot = previous;
        }

        return origin.Move(delta);
    }

    private static int SumBoxGpsCoordinates(CellType[,] grid) =>
        Enumerable
            .Range(0, grid.GetLength(0))
            .Zip(Enumerable.Range(0, grid.GetLength(1)))
            .Where(pair => grid[pair.First, pair.Second] == CellType.Box)
            .Sum(pair => GpsCoordinate(pair.First, pair.Second));

    private static int GpsCoordinate(int r, int c) => r * 100 + c;
}

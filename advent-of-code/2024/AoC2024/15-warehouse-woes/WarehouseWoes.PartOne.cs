using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static int PartOne(string filePath)
    {
        var (grid, robotPosition) = ParseGrid(filePath, false);

        foreach (var direction in ParseMoves(filePath))
            robotPosition = Move(grid, robotPosition, direction);

        return SumBoxGpsCoordinates(grid);
    }

    private static Coordinate Move(CellType[,] grid, Coordinate origin, Direction direction)
    {
        Coordinate? maybeFreeSpot = null;

        var delta = direction.ToDelta();
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
        Enumerable.Range(0, grid.GetLength(0))
            .SelectMany(r => Enumerable.Range(0, grid.GetLength(1))
                .Select(c => new Coordinate(r, c)))
            .Where(coordinate => grid[coordinate.R, coordinate.C] == CellType.Box)
            .Sum(WarehouseWoesExtensions.ToGpsCoordinate);
}

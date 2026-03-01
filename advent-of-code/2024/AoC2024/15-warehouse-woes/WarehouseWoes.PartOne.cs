using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static int PartOne(string filePath) => MoveAndSumGps(filePath, false);

    private static int MoveAndSumGps(string filePath, bool isWideVersion)
    {
        var (grid, robotPosition) = ParseGrid(filePath, isWideVersion);

        foreach (var direction in ParseMoves(filePath))
            robotPosition = Move(grid, robotPosition, direction);

        return SumBoxGpsCoordinates(grid);
    }

    private static Coordinate Move(CellType[,] grid, Coordinate origin, Direction direction)
    {
        IEnumerable<Coordinate>? maybeFreeSpots = null;

        var delta = direction.ToDelta();
        Delta reverseDelta = delta.Reverse();
        Delta leftDelta = new(0, -1);
        Delta rightDelta = leftDelta.Reverse();

        Coordinate[] start = [ origin ];
        IEnumerable<Coordinate> nextCoords = start.Select(coordinate => coordinate.Move(delta));
        while (nextCoords.All(coord => coord.IsInBounds(grid)))
        {
            var cellTypes = nextCoords.Select(coord => grid[coord.R, coord.C]);
            if (cellTypes.Any(ct => ct == CellType.Wall))
                break;

            if (cellTypes.All(ct => ct == CellType.Free))
            {
                maybeFreeSpots = nextCoords;
                break;
            }

            nextCoords = nextCoords.SelectMany(coord =>
            {
                var ct = grid[coord.R, coord.C];

                Coordinate[] res = ct switch
                {
                    CellType.Box => [coord.Move(delta)],
                    CellType.BoxStart => [coord.Move(delta), coord.Move(rightDelta).Move(delta)],
                    CellType.BoxEnd => [coord.Move(delta), coord.Move(leftDelta).Move(delta)],
                    CellType.Wall => throw new ArgumentException("Walls should have exited the loop already"),
                    CellType.Free => [],
                    _ => throw ExhaustiveMatch.Failed(ct)
                };
                return res;
            });            
        }

        if (maybeFreeSpots is not IEnumerable<Coordinate> freeSpots)
            return origin;
        
        if (!freeSpots.Any())
            return origin;
        
        while (freeSpots.Any(coord => coord != origin))
        {
            var previousSpots = freeSpots.Select(coord => coord.Move(reverseDelta));
            freeSpots
                .Zip(previousSpots)
                .ToList()
                .ForEach(pair =>
                {
                    grid[pair.First.R, pair.First.C] = grid[pair.Second.R, pair.Second.C];
                });
            freeSpots = previousSpots;
        }

        return origin.Move(delta);
    }

    private static int SumBoxGpsCoordinates(CellType[,] grid) =>
        Enumerable.Range(0, grid.GetLength(0))
            .SelectMany(r => Enumerable.Range(0, grid.GetLength(1))
                .Select(c => new Coordinate(r, c)))
            .Where(coordinate => grid[coordinate.R, coordinate.C] is CellType.Box or CellType.BoxStart)
            .Sum(WarehouseWoesExtensions.ToGpsCoordinate);
}

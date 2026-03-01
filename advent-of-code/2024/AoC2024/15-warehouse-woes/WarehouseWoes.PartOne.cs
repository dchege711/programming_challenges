using System.Diagnostics;
using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static int PartOne(string filePath) => MoveAndSumGps(filePath, false);

    public static int PartTwo(string filePath) => MoveAndSumGps(filePath, true);

    private static int MoveAndSumGps(string filePath, bool isWideVersion)
    {
        var (grid, robotPosition) = ParseGrid(filePath, isWideVersion);

        Visualize(grid, robotPosition);
        foreach (var direction in ParseMoves(filePath))
        {
            Debug.WriteLine($"Moving {direction}");
            robotPosition = Move(grid, robotPosition, direction);
            Visualize(grid, robotPosition);
        }

        return SumBoxGpsCoordinates(grid);
    }

    private static Coordinate Move(CellType[,] grid, Coordinate origin, Direction direction)
    {
        IEnumerable<Coordinate>? maybeFreeSpots = null;

        var delta = direction.ToDelta();
        var isLateralMove = direction.IsLateral();
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
                    CellType.BoxStart => isLateralMove ? [coord.Move(delta)] : [coord.Move(delta), coord.Move(rightDelta).Move(delta)],
                    CellType.BoxEnd => isLateralMove ? [coord.Move(delta)] : [coord.Move(delta), coord.Move(leftDelta).Move(delta)],
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

    private static void Visualize(CellType[,] grid, Coordinate robotPosition)
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

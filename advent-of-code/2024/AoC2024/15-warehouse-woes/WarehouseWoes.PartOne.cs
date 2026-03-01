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
        var delta = direction.ToDelta();
        var isLateralMove = direction.IsLateral();
        Delta reverseDelta = delta.Reverse();
        Delta leftDelta = new(0, -1);
        Delta rightDelta = leftDelta.Reverse();

        IEnumerable<Coordinate> moveFrontier = [];

        IEnumerable<Coordinate> targetCoords = [ origin.Move(delta) ];
        while (targetCoords.All(coord => coord.IsInBounds(grid)))
        {
            var cellTypes = targetCoords.Select(coord => grid[coord.R, coord.C]);
            if (cellTypes.Any(ct => ct == CellType.Wall))
                break;

            if (cellTypes.All(ct => ct == CellType.Free))
            {
                moveFrontier = targetCoords;
                break;
            }

            targetCoords = targetCoords.SelectMany(coord =>
            {
                Coordinate[] res = grid[coord.R, coord.C] switch
                {
                    CellType.Box => [coord.Move(delta)],

                    CellType.BoxStart => isLateralMove
                        ? [coord.Move(delta)]
                        : [coord.Move(delta), coord.Move(rightDelta).Move(delta)],

                    CellType.BoxEnd => isLateralMove
                        ? [coord.Move(delta)]
                        : [coord.Move(delta), coord.Move(leftDelta).Move(delta)],

                    CellType.Wall => throw new ArgumentException(
                        "Walls should have exited the loop already"),

                    CellType.Free => [],

                    _ => throw ExhaustiveMatch.Failed(grid[coord.R, coord.C])
                };
                return res;
            });
        }

        if (!moveFrontier.Any())
            return origin;

        while (moveFrontier.Any(coord => coord != origin))
        {
            Debug.WriteLine($"Move frontier: {string.Join(',', moveFrontier.Select(p => $"({p.R}, {p.C})"))}");
            HashSet<Coordinate> previousMoveFrontier = [];
            foreach (var target in moveFrontier)
            {
                var source = target.Move(reverseDelta);
                var sourceCellType = grid[source.R, source.C];
                switch (sourceCellType)
                {
                    case CellType.Box:
                        {
                            grid[target.R, target.C] = sourceCellType;
                            grid[source.R, source.C] = CellType.Free;
                            previousMoveFrontier.Add(source);
                            break;
                        }

                    case CellType.BoxStart:
                        {
                            var canMove = isLateralMove || moveFrontier.Contains(target.Move(rightDelta));
                            if (!canMove)
                                break;

                            grid[target.R, target.C] = sourceCellType;
                            grid[source.R, source.C] = CellType.Free;
                            previousMoveFrontier.Add(source);
                            break;
                        }

                    case CellType.BoxEnd:
                        {
                            var canMove = isLateralMove || moveFrontier.Contains(target.Move(leftDelta));
                            if (!canMove)
                                break;

                            grid[target.R, target.C] = sourceCellType;
                            grid[source.R, source.C] = CellType.Free;
                            previousMoveFrontier.Add(source);
                            break;
                        }

                    case CellType.Free:
                        break;

                    case CellType.Wall:
                        throw new ArgumentException("Walls should have exited the loop already");

                    default:
                        throw ExhaustiveMatch.Failed(sourceCellType);
                };
            }
            Visualize(grid, origin);

            moveFrontier = previousMoveFrontier;
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

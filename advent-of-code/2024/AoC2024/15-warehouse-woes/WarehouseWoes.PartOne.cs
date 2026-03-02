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
        var grid = ParseGrid(filePath, isWideVersion);
        grid.Visualize();

        foreach (var direction in ParseMoves(filePath))
        {
            Debug.WriteLine($"Moving {direction} from {grid.RobotPosition}");
            grid = Move(grid, direction);
            grid.Visualize();
        }

        return SumBoxGpsCoordinates(grid);
    }

    private static Grid Move(Grid grid, Direction direction)
    {
        var origin = grid.RobotPosition;

        var delta = direction.ToDelta();
        var isLateralMove = direction.IsLateral();
        Delta reverseDelta = delta.Reverse();

        IEnumerable<Coordinate> moveFrontier = [];

        HashSet<Coordinate> frontierCandidates = [origin.Add(delta)];
        HashSet<Coordinate> visited = [];
        while (frontierCandidates.All(grid.IsInBounds))
        {
            Debug.WriteLine($"Expanding frontier: {string.Join(',', frontierCandidates.Select(p => $"({p.R}, {p.C})"))}");
            if (frontierCandidates.Any(grid.IsWall))
                break;

            if (frontierCandidates.All(grid.IsFree))
            {
                moveFrontier = frontierCandidates;
                break;
            }

            var newFrontierCandidates = frontierCandidates
                .SelectMany(coord =>
                    {
                        var nextTarget = coord.Add(delta);
                        Coordinate[] res = grid.GetCellType(coord) switch
                        {
                            CellType.Box => [nextTarget],
                            CellType.BoxStart => [nextTarget, nextTarget.Add(RightDelta)],
                            CellType.BoxEnd => [nextTarget, nextTarget.Add(LeftDelta)],
                            CellType.Wall => throw new ArgumentException("Walls should be frontier candidates"),
                            CellType.Free => [],
                            _ => throw ExhaustiveMatch.Failed(grid.GetCellType(coord))
                        };
                        return res;
                    })
                .ToHashSet();

            if (frontierCandidates.SetEquals(newFrontierCandidates))
                break;

            frontierCandidates = newFrontierCandidates;
        }

        Debug.WriteLine($"Move frontier: {string.Join(',', moveFrontier.Select(p => $"({p.R}, {p.C})"))}");
        if (!moveFrontier.Any())
            return grid;

        while (moveFrontier.Any(coord => coord != origin))
        {
            Debug.WriteLine($"Move frontier: {string.Join(',', moveFrontier.Select(p => $"({p.R}, {p.C})"))}");
            HashSet<Coordinate> nextMoveFrontier = [];
            foreach (var outerTarget in moveFrontier)
            {
                if (!grid.IsFree(outerTarget))
                    throw new InvalidOperationException($"Frontier ({outerTarget.R}, {outerTarget.C}) is not empty: {grid.GetCellType(outerTarget)}");

                var innerSource = outerTarget.Add(reverseDelta);
                var sourceCellType = grid.GetCellType(innerSource);
                switch (sourceCellType)
                {
                    case CellType.Box:
                        {
                            grid.Boxes.Remove(innerSource);
                            grid.Boxes.Add(outerTarget);
                            nextMoveFrontier.Add(innerSource);
                            break;
                        }

                    case CellType.BoxStart:
                        {
                            var outerTargetBoxEnd = outerTarget.Add(RightDelta);
                            var canMove = isLateralMove || grid.IsFree(outerTargetBoxEnd);
                            if (!canMove)
                                break;

                            grid.Boxes.Remove(innerSource);
                            grid.Boxes.Add(outerTarget);

                            nextMoveFrontier.Add(
                                isLateralMove ? outerTarget.Add(RightDelta).Add(RightDelta) : innerSource);
                            break;
                        }

                    case CellType.BoxEnd:
                        break;

                    case CellType.Free:
                        break;

                    case CellType.Wall:
                        throw new ArgumentException("Walls should have exited the loop already");

                    default:
                        throw ExhaustiveMatch.Failed(sourceCellType);
                };
            }
            moveFrontier = nextMoveFrontier;
        }

        return grid with { RobotPosition = origin.Add(delta) };
    }

    private static int SumBoxGpsCoordinates(Grid grid) =>
        grid.Boxes.Sum(WarehouseWoesExtensions.ToGpsCoordinate);

    private static Delta LeftDelta = new(0, -1);
    private static Delta RightDelta = new(0, 1);
}

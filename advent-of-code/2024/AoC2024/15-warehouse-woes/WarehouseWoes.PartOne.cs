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
            Debug.WriteLine($"Moving {direction}");
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
        Delta leftDelta = new(0, -1);
        Delta rightDelta = leftDelta.Reverse();

        IEnumerable<Coordinate> moveFrontier = [];

        HashSet<Coordinate> targetCoords = [origin.Add(delta)];
        while (targetCoords.All(grid.IsInBounds))
        {
            if (targetCoords.Any(grid.IsWall))
                break;

            if (targetCoords.All(grid.IsFree))
            {
                moveFrontier = targetCoords;
                break;
            }

            targetCoords = targetCoords
                .Select(coord =>
                    {
                        var nextTarget = coord.Add(delta);
                        Coordinate? res = grid.GetCellType(coord) switch
                        {
                            CellType.Box => nextTarget,
                            CellType.BoxStart => nextTarget,
                            CellType.BoxEnd => nextTarget.Add(leftDelta),
                            CellType.Wall => throw new ArgumentException(
                                "Walls should have exited the loop already"),
                            CellType.Free => null,
                            _ => throw ExhaustiveMatch.Failed(grid.GetCellType(coord))
                        };
                        return res;
                    })
                .OfType<Coordinate>()
                .ToHashSet();
        }

        if (!moveFrontier.Any())
            return grid;

        while (moveFrontier.Any(coord => coord != origin))
        {
            Debug.WriteLine($"Move frontier: {string.Join(',', moveFrontier.Select(p => $"({p.R}, {p.C})"))}");
            HashSet<Coordinate> nextMoveFrontier = [];
            foreach (var outerTarget in moveFrontier)
            {
                Debug.WriteLine($"Processing {outerTarget} from the move frontier");
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
                        throw new ArgumentException($"BoxEnds should not be in the frontier; found {innerSource}");

                    case CellType.Free:
                        break;

                    case CellType.Wall:
                        throw new ArgumentException("Walls should have exited the loop already");

                    default:
                        throw ExhaustiveMatch.Failed(sourceCellType);
                };
                grid.Visualize();
            }
            moveFrontier = nextMoveFrontier;
        }

        return grid with { RobotPosition = origin.Add(delta) };
    }

    private static int SumBoxGpsCoordinates(Grid grid) =>
        grid.Boxes.Sum(WarehouseWoesExtensions.ToGpsCoordinate);

    private static Delta RightDelta = new(0, 1);
}

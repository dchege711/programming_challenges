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
            Move(grid, direction);
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

        IEnumerable<Coordinate> targetCoords = [ origin.Move(delta) ];
        while (targetCoords.All(grid.IsInBounds))
        {
            if (targetCoords.Any(grid.IsWall))
                break;

            if (targetCoords.All(grid.IsFree))
            {
                moveFrontier = targetCoords;
                break;
            }

            targetCoords = targetCoords.SelectMany(coord =>
            {
                Coordinate[] res = grid.GetCellType(coord) switch
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

                    _ => throw ExhaustiveMatch.Failed(grid.GetCellType(coord))
                };
                return res;
            });
        }

        if (!moveFrontier.Any())
            return grid;

        while (moveFrontier.Any(coord => coord != origin))
        {
            Debug.WriteLine($"Move frontier: {string.Join(',', moveFrontier.Select(p => $"({p.R}, {p.C})"))}");
            HashSet<Coordinate> previousMoveFrontier = [];
            foreach (var target in moveFrontier)
            {
                var source = target.Move(reverseDelta);
                var sourceCellType = grid.GetCellType(source);
                switch (sourceCellType)
                {
                    case CellType.Box:
                        {
                            grid.Boxes.Remove(source);
                            grid.Boxes.Add(target);
                            previousMoveFrontier.Add(source);
                            break;
                        }

                    case CellType.BoxStart:
                        {
                            var canMove = isLateralMove || moveFrontier.Contains(target.Move(rightDelta));
                            if (!canMove)
                                break;

                            grid.Boxes.Remove(source);
                            grid.Boxes.Add(target);
                            previousMoveFrontier.Add(source);
                            break;
                        }

                    case CellType.BoxEnd:
                        {
                            var canMove = isLateralMove || moveFrontier.Contains(target.Move(leftDelta));
                            if (!canMove)
                                break;

                            grid.Boxes.Remove(source);
                            grid.Boxes.Add(target);
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
            grid.Visualize();
            moveFrontier = previousMoveFrontier;
        }

        return grid with { RobotPosition = origin.Move(delta) };
    }

    private static int SumBoxGpsCoordinates(Grid grid) =>
        grid.Boxes.Sum(WarehouseWoesExtensions.ToGpsCoordinate);
}

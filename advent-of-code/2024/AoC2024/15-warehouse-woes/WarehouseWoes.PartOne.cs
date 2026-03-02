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

            frontierCandidates = frontierCandidates
                .SelectMany(coord => GetNextFrontier(grid, coord, direction, delta))
                .ToHashSet();
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

    private static IEnumerable<Coordinate> GetNextFrontier(Grid grid, Coordinate start, Direction direction, Delta delta)
    {
        var target = start.Add(delta);
        var cellType = grid.GetCellType(start);
        Debug.WriteLine($"Evaluating next frontier for {start} -> {target} w/ start={cellType}");
        IEnumerable<Coordinate> res = cellType switch
        {
            CellType.Box => [target],
            CellType.BoxStart => [target],
            CellType.BoxEnd => GetNextFrontierForBoxEnd(grid, target, direction),
            CellType.Wall => [],
            CellType.Free => [target],
            _ => throw ExhaustiveMatch.Failed(cellType)
        };
        return res;
    }

    private static IEnumerable<Coordinate> GetNextFrontierForBoxEnd(
        Grid grid, Coordinate candidate, Direction direction)
    {
        Debug.WriteLine($"Evaluating pickups for BoxEnd at {candidate}");
        if (grid.GetCellType(candidate) is not CellType.BoxEnd)
            throw new ArgumentException($"Expected BoxEnd at {candidate} but got {grid.GetCellType(candidate) }");

        var boxStartCell = candidate.Add(LeftDelta);

        if (direction.IsLateral())
            return [boxStartCell];

        var pickAlong = candidate.Add(RightDelta);
        if (!grid.IsInBounds(pickAlong))
            return [boxStartCell];

        var pickAlongCellType = grid.GetCellType(pickAlong);
        IEnumerable<Coordinate> res = pickAlongCellType switch
        {
            CellType.Box => throw new InvalidDataException("Cannot mix Box w/ BoxEnd"),
            CellType.BoxStart => [boxStartCell, pickAlong],
            CellType.BoxEnd => throw new InvalidDataException($"{pickAlong} next to BoxEnd {candidate} cannot also be a BoxEnd"),
            CellType.Wall => [boxStartCell, pickAlong],
            CellType.Free => [boxStartCell],
            _ => throw ExhaustiveMatch.Failed(pickAlongCellType)
        };
        return res;
    }

    private static int SumBoxGpsCoordinates(Grid grid) =>
        grid.Boxes.Sum(WarehouseWoesExtensions.ToGpsCoordinate);

    private static Delta LeftDelta = new(0, -1);
    private static Delta RightDelta = new(0, 1);
}

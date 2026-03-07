using System.Collections.Immutable;
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

        grid.Visualize(robotPosition);
        foreach (var direction in ParseMoves(filePath))
        {
            Debug.WriteLine($"Moving {direction}");
            robotPosition = Move(grid, robotPosition, direction);
            grid.Visualize(robotPosition);
        }

        return grid.SumBoxGpsCoordinates();
    }

    private static Coordinate Move(CellType[,] grid, Coordinate origin, Direction direction)
    {
        var delta = direction.ToDelta();
        var isLateralMove = direction.IsLateral();

        var cellsToShiftOver = GetCellsToShiftOver(grid, origin, direction);
        Debug.WriteLine($"Shifting Over: {string.Join(',', cellsToShiftOver.Reverse())}");
        if (cellsToShiftOver.Count == 0)
            return origin;

        foreach (var source in cellsToShiftOver.Reverse())
        {
            var target = source + delta;
            grid[target.R, target.C] = grid[source.R, source.C];
            grid[source.R, source.C] = CellType.Free;
        }

        return origin + delta;
    }

    private static IReadOnlyList<Coordinate> GetCellsToShiftOver(CellType[,] grid, Coordinate origin, Direction direction)
    {
        var delta = direction.ToDelta();
        var isLateralMove = direction.IsLateral();

        IReadOnlyList<Coordinate> candidates = GetCellsAffectedByMove(grid, origin, direction);
        var cellsToShift = ImmutableList.CreateBuilder<Coordinate>();
        cellsToShift.Add(origin);
        while (candidates.All(coord => coord.IsInBounds(grid)))
        {
            var cellTypes = candidates.Select(coord => grid[coord.R, coord.C]).ToArray();

            if (cellTypes.Any(ct => ct is CellType.Wall))
                return [];

            if (cellTypes.All(ct => ct is CellType.Free))
                return cellsToShift.ToList();

            cellsToShift.AddRange(candidates);
            candidates = candidates
                .SelectMany(coord => GetCellsAffectedByMove(grid, coord, direction))
                .ToList();
        }

        return [];
    }

    private static IReadOnlyList<Coordinate> GetCellsAffectedByMove(
        CellType[,] grid, Coordinate origin, Direction direction)
    {
        var delta = direction.ToDelta();
        var target = origin + delta;

        if (direction.IsLateral())
            return [target];

        if (!target.IsInBounds(grid))
            return [target];

        var cellType = grid[target.R, target.C];
        IReadOnlyList<Coordinate> affectedCells = cellType switch
        {
            CellType.Wall => [target],
            CellType.Box => [target],
            CellType.Free => [target],
            CellType.BoxStart => [target, target + RightDelta],
            CellType.BoxEnd => [target + LeftDelta, target],
            _ => throw ExhaustiveMatch.Failed(cellType)
        };

        Debug.WriteLine($"Moving {origin} {direction} affects: {string.Join(',', affectedCells)}");
        return affectedCells;
    }

    private static Delta LeftDelta = new(0, -1);
    private static Delta RightDelta = new(0, 1);
}

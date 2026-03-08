using ExhaustiveMatching;
using AoC2024.WarehouseWoesDataTypes;
using System.Collections.Immutable;

namespace AoC2024;

public partial class WarehouseWoes
{
    public CellType[,] Grid { get; }
    public Coordinate RobotPosition { get; private set; }

    private IReadOnlyList<Direction> Directions;

    public WarehouseWoes(string filePath, bool isWideVersion)
    {
        using StreamReader inputReader = new(filePath);

        List<CellType[]> rows = [];
        string? line = null;
        Coordinate? maybeRobotPosition = null;
        while ((line = inputReader.ReadLine()) != null)
        {
            if (line.Length == 0)
                break;

            if (maybeRobotPosition is null)
            {
                var c = line.IndexOf('@');
                if (c != -1)
                    maybeRobotPosition = new(rows.Count, c * (isWideVersion ? 2 : 1));
            }

            rows.Add(line.SelectMany(c => ToCellTypes(c, isWideVersion)).ToArray());
        }

        if (maybeRobotPosition is not Coordinate robotPosition)
            throw new ArgumentException("Did not find starting position");

        RobotPosition = robotPosition;

        int R = rows.Count;
        if (R <= 0)
            throw new ArgumentException("Did not find a grid");

        int C = rows[0].Length;
        Grid = new CellType[R, C];
        for (int r = 0; r < R; r++)
            for (int c = 0; c < C; c++)
                Grid[r, c] = rows[r][c];

        var directions = ImmutableArray.CreateBuilder<Direction>();
        while ((line = inputReader.ReadLine()) != null)
            directions.AddRange(line.Select(ToDirection));
        Directions = directions.ToArray();
    }

    private static Direction ToDirection(char c) => c switch
    {
        '<' => Direction.Left,
        '>' => Direction.Right,
        '^' => Direction.Up,
        'v' => Direction.Down,
        _ => throw new ArgumentException($"{c} cannot be parsed into a Direction.")
    };

    private static CellType[] ToCellTypes(char c, bool isWideVersion) =>
        isWideVersion ? ToCellTypesWide(c) : ToCellTypesNarrow(c);

    private static CellType[] ToCellTypesNarrow(char c) => c switch
    {
        '#' => [CellType.Wall],
        'O' => [CellType.Box],
        '[' => [CellType.BoxStart],
        ']' => [CellType.BoxEnd],
        '.' => [CellType.Free],
        '@' => [CellType.Free],
        _ => throw new ArgumentException($"{c} cannot be parsed into a CellType.")
    };

    private static CellType[] ToCellTypesWide(char c) => c switch
    {
        '#' => [CellType.Wall, CellType.Wall],
        'O' => [CellType.BoxStart, CellType.BoxEnd],
        '.' => [CellType.Free, CellType.Free],
        '@' => [CellType.Free, CellType.Free],
        _ => throw new ArgumentException($"{c} cannot be parsed into a CellType.")
    };
}

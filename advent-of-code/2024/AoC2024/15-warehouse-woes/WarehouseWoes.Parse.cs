using ExhaustiveMatching;
using AoC2024.WarehouseWoesDataTypes;
using System.Collections.Immutable;

namespace AoC2024;

public partial class WarehouseWoes
{
    public CellType[,] Grid { get; }
    public Coordinate RobotPosition { get; private set; }

    private readonly IReadOnlyList<Direction> Directions;

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

        List<Direction> directions = [];
        while ((line = inputReader.ReadLine()) != null)
            directions.AddRange(line.Select(ToDirection));
        Directions = directions;
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
        '#' => NarrowWall,
        'O' => NarrowBox,
        '[' => NarrowBoxStart,
        ']' => NarrowBoxEnd,
        '.' => NarrowFree,
        '@' => NarrowFree,
        _ => throw new ArgumentException($"{c} cannot be parsed into a CellType.")
    };

    private static CellType[] ToCellTypesWide(char c) => c switch
    {
        '#' => WideWall,
        'O' => WideBox,
        '.' => WideFree,
        '@' => WideFree,
        _ => throw new ArgumentException($"{c} cannot be parsed into a CellType.")
    };

    private static readonly CellType[] NarrowWall = [CellType.Wall];
    private static readonly CellType[] NarrowBox = [CellType.Box];
    private static readonly CellType[] NarrowFree = [CellType.Free];
    private static readonly CellType[] NarrowBoxStart = [CellType.BoxStart];
    private static readonly CellType[] NarrowBoxEnd = [CellType.BoxEnd];
    private static readonly CellType[] WideWall = [CellType.Wall, CellType.Wall];
    private static readonly CellType[] WideBox = [CellType.BoxStart, CellType.BoxEnd];
    private static readonly CellType[] WideFree = [CellType.Free, CellType.Free];
}

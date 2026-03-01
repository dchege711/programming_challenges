using ExhaustiveMatching;
using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static (CellType[,], Coordinate) ParseGrid(string filePath, bool isWideVersion)
    {
        using StreamReader inputReader = new(filePath);

        List<CellType[]> rows = [];
        string? line = null;
        Coordinate? maybeStartingPosition = null;
        while ((line = inputReader.ReadLine()) != null)
        {
            if (line.Length == 0)
                break;
            
            if (maybeStartingPosition is null)
            {
                var c = line.IndexOf('@');
                if (c != -1)
                   maybeStartingPosition = new(rows.Count, c * (isWideVersion ? 2 : 1));
            }

            rows.Add([.. line.SelectMany(c => ToCellTypes(c, isWideVersion))]);
        }

        if (maybeStartingPosition is not Coordinate startingPosition)
            throw new ArgumentException("Did not find starting position");

        int R = rows.Count;
        int C = rows.First().Length;

        var grid = new CellType[R, C];
        for (int r = 0; r < R; r++)
            for (int c = 0; c < C; c++)
                grid[r, c] = rows[r][c];

        return (grid, startingPosition);
    }

    public static IEnumerable<Direction> ParseMoves(string filePath)
    {
        using var inputReader = new StreamReader(filePath);

        string? line = null;
        while ((line = inputReader.ReadLine()) != null)
        {
            if (line.Length == 0)
                break;
        }

        while ((line = inputReader.ReadLine()) != null)
        {
            foreach (var move in line.Select(ToMove))
                yield return move;
        }
    }

    private static Direction ToMove(char c)
    {
        switch (c)
        {
            case '<': return Direction.Left;
            case '>': return Direction.Right;
            case '^': return Direction.Up;
            case 'v': return Direction.Down;
        }
        
        throw ExhaustiveMatch.Failed(c);
    }

    private static IEnumerable<CellType> ToCellTypes(char c, bool isWideVersion) =>
        isWideVersion ? ToCellTypesWide(c) : ToCellTypesNarrow(c);

    private static IEnumerable<CellType> ToCellTypesNarrow(char c)
    {
        switch (c)
        {
            case '#': return [CellType.Wall];
            case 'O': return [CellType.Box];
            case '.': return [CellType.Free];
            case '@': return [CellType.Free];
        }
        
        throw ExhaustiveMatch.Failed(c);
    }

    private static IEnumerable<CellType> ToCellTypesWide(char c)
    {
        switch (c)
        {
            case '#': return [CellType.Wall, CellType.Wall];
            case 'O': return [CellType.BoxStart, CellType.BoxEnd];
            case '.': return [CellType.Free, CellType.Free];
            case '@': return [CellType.Free, CellType.Free];
        }
        
        throw ExhaustiveMatch.Failed(c);
    }
}

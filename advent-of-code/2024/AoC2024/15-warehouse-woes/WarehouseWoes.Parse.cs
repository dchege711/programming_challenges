using ExhaustiveMatching;
using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static (CellType[,], Coordinate) ParseGrid(string filePath)
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
                   maybeStartingPosition = new(rows.Count, c);
            }

            rows.Add([.. line.Select(ToCellType)]);
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

    public static IEnumerable<Move> ParseMoves(string filePath)
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

    private static Move ToMove(char c)
    {
        switch (c)
        {
            case '<': return Move.Left;
            case '>': return Move.Right;
            case '^': return Move.Up;
            case 'v': return Move.Down;
        }
        
        throw ExhaustiveMatch.Failed(c);
    }

    private static CellType ToCellType(char c)
    {
        switch (c)
        {
            case '#': return CellType.Wall;
            case 'O': return CellType.Box;
            case '.': return CellType.Free;
            case '@': return CellType.Free;
        }
        
        throw ExhaustiveMatch.Failed(c);
    }
}

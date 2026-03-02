using ExhaustiveMatching;
using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024;

public partial class WarehouseWoes
{
    public static Grid ParseGrid(string filePath, bool isWideVersion)
    {
        using StreamReader inputReader = new(filePath);

        string? line = null;
        Coordinate? maybeStartingPosition = null;
        int rowIndex = -1;
        int? maybeColCount = null;
        HashSet<Coordinate> walls = [];
        HashSet<Coordinate> boxes = [];
        while ((line = inputReader.ReadLine()) != null)
        {
            if (line.Length == 0)
                break;

            rowIndex++;

            line = isWideVersion ? ExpandGridLine(line) : line;
            if (maybeColCount is null)
            {
                maybeColCount = line.Length;
            }
            else if (line.Length != maybeColCount)
            {
                throw new ArgumentException(
                    $"Mismatch in column count {line.Length} vs. {maybeColCount}");
            }

            if (maybeStartingPosition is null)
            {
                var cIdx = line.IndexOf('@');
                if (cIdx != -1)
                   maybeStartingPosition = new(rowIndex, cIdx);
            }

            walls.UnionWith(GetWalls(line, rowIndex));
            boxes.UnionWith(GetBoxes(line, rowIndex));
        }

        int rowCount = rowIndex + 1;
        if (rowCount <= 0)
            throw new ArgumentException("Did not parse any rows");

        if (maybeColCount is not int colCount)
            throw new ArgumentException("Did not establish the column count");

        if (maybeStartingPosition is not Coordinate startingPosition)
            throw new ArgumentException("Did not find starting position");

        return new(rowCount, colCount, startingPosition, walls, boxes, isWideVersion ? 2 : 1);
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

    private static string ExpandGridLine(string s)
    {
        return string.Concat(s.Select(c =>
        {
            switch (c)
            {
                case '#': return "##";
                case 'O': return "[]";
                case '.': return "..";
                case '@': return "@.";
            }
            throw ExhaustiveMatch.Failed(c);
        }));
    }

    private static IEnumerable<Coordinate> GetWalls(string s, int r) =>
        s.Index().Where(p => p.Item == '#').Select(p => new Coordinate(r, p.Index));

    private static IEnumerable<Coordinate> GetBoxes(string s, int r) =>
        s.Index().Where(p => p.Item is 'O' or '[').Select(p => new Coordinate(r, p.Index));
}

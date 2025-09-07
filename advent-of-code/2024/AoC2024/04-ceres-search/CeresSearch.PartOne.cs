using System.Collections.Immutable;

namespace AoC2024;

public partial class CeresSearch
{
    static private ImmutableList<(int, int)> possibleMoves = ImmutableList.Create(
        (-1, 0),    // Up
        (-1, 1),    // Up-Right
        (0, 1),     // Right
        (1, 1),     // Down-Right
        (1, 0),     // Down
        (1, -1),    // Down-Left
        (0, -1),    // Left
        (1, -1)     // Up-Left
    );

    public int PartOne()
    {
        int numOccurrences = 0;
        foreach (var (r, row) in grid.Index())
        {
            foreach (var (c, val) in row.Index())
            {
                if (val == 'X')
                {
                    Coordinate starting = new(r, c);
                    numOccurrences += NumOccurrences("X", starting, [starting]);
                }
            }
        }
        return numOccurrences;
    }

    private int NumOccurrences(
        string prefix,
        Coordinate current,
        ImmutableHashSet<Coordinate> visited)
    {
        var targetChar = ComputeNextChar(prefix);
        if (targetChar is null)
            return 1;
    
        var newPrefix = prefix + targetChar;
        var numOccurrences = 0;
        foreach (var coordinate in MovesAvailable(current, visited))
        {
            if (grid[coordinate.R][coordinate.C] != targetChar)
                continue;
            
            numOccurrences += NumOccurrences(
                newPrefix, coordinate, visited.Add(coordinate));
        }

        return numOccurrences;
    }

    private IEnumerable<Coordinate> MovesAvailable(
        Coordinate current,
        ImmutableHashSet<Coordinate> visited)
    {
        foreach (var (deltaR, deltaC) in possibleMoves)
        {
            Coordinate next = new(current.R + deltaR, current.C + deltaC);
            if (visited.Contains(next))
                continue;
            
            if (next.R >= 0 && next.R < numRows && next.C >= 0 && next.C < numCols)
                yield return next;
        }
    }

    private static char? ComputeNextChar(string prefix)
    {
        const string target = "XMAS";

        if (!target.StartsWith(prefix))
            throw new NotSupportedException($"{prefix} is not a prefix of 'XMAS'");
        
        if (prefix.Length == target.Length)
            return null;
        
        return "XMAS".ElementAt(prefix.Length);
    }

    private readonly record struct Coordinate(int R, int C);
}

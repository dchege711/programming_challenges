using System.Collections.Immutable;
using System.Diagnostics;

namespace AoC2024;

public partial class CeresSearch
{
    private readonly static string crossTargetString = "MAS";
    private readonly static ImmutableHashSet<string> crossTargetStringVariations = [
        crossTargetString, string.Concat(crossTargetString.Reverse())];
    
    private readonly static int crossTargetStringRadius = crossTargetString.Length / 2;

    static CeresSearch()
    {
        Debug.Assert(crossTargetString.Length % 2 == 1, 
            $"{nameof(crossTargetString)} must be an odd-length string");
    }

    public int PartTwo()
    {
        int numOccurrences = 0;
        for (var r = crossTargetStringRadius; r < numRows - crossTargetStringRadius; r++)
            for (var c = crossTargetStringRadius; c < numCols - crossTargetStringRadius; c++)
                numOccurrences += HasCrossOccurence(r, c) ? 1 : 0;

        return numOccurrences;
    }

    private bool HasCrossOccurence(int r, int c)
    {        
        if (grid[r][c] != crossTargetString[crossTargetStringRadius])
            return false;

        var crossPaths = GetCrossPaths(r, c);
        if (!crossPaths.Any())
            return false;
        
        return crossPaths.All(coordinates => {
            var chars = coordinates.Select(coordinate => grid[coordinate.R][coordinate.C]);            
            return crossTargetStringVariations.Contains(string.Concat(chars));
        });
    }

    private IEnumerable<IEnumerable<Coordinates>> GetCrossPaths(int r, int c)
    {
        int topR = r - crossTargetStringRadius;
        int bottomR = r + crossTargetStringRadius;
        int leftC = c - crossTargetStringRadius;
        int rightC = c + crossTargetStringRadius;

        if (topR < 0 || bottomR >= numRows || leftC < 0 || rightC >= numCols)
            yield break;

        yield return GetRange(new(topR, leftC), new(1, 1), crossTargetString.Length);
        yield return GetRange(new(topR, rightC), new(1, -1), crossTargetString.Length);
    }

    private static IEnumerable<Coordinates> GetRange(
        Coordinates start,
        Coordinates delta,
        int count)
    {
        for (var i = 0; i < count; i++)
            yield return new(start.R + i * delta.R, start.C + i * delta.C);
    }

    private readonly record struct Coordinates(int R, int C);
}

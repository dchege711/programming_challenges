using System.Collections.Immutable;

namespace AoC2024;

public static partial class CeresSearch
{
    public static char[][] ParseWordSearch(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        List<char[]> grid = [];
        string? line;
        while((line = inputReader.ReadLine()) != null)
        {
            grid.Add(line.ToCharArray());
        }
        return [.. grid];
    }
}

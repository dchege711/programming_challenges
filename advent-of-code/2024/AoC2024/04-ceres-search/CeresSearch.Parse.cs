namespace AoC2024;

public partial class CeresSearch
{
    private readonly char[][] grid;
    private readonly int numRows;
    private readonly int numCols;

    public CeresSearch(string filePath)
    {
        grid = ParseWordSearch(filePath);
        numRows = grid.Length;
        numCols = grid.First().Length;
    }

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

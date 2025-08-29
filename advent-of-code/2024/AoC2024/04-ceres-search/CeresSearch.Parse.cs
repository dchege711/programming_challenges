namespace AoC2024;

public static partial class CeresSearch
{
    public static IEnumerable<string> ParseWordSearch(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;
        while((line = inputReader.ReadLine()) != null)
        {
            yield return line;
        }
    }
}

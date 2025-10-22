namespace AoC2024;

public partial class HoofIt
{
    public sealed record Coordinate(int r, int c);
    public sealed record TopographicMap(
        int[,] Map, IReadOnlyList<Coordinate> TrailEnds);

    public static TopographicMap Parse(string filePath)
    {
        var lines = File.ReadLines(filePath).ToList();
    
        int rowCount = lines.Count;
        int colCount = lines.First().Length;
        int[,] map = new int[rowCount, colCount];
        List<Coordinate> trailEnds = [];

        for (int r = 0; r < rowCount; r++)
        {
            for (int c = 0; c < colCount; c++)
            {
                int height = lines[r][c] - '0';
                map[r, c] = height;
                if (height == TrailEndHeight)
                    trailEnds.Add(new(r, c));
            }
        }

        return new(map, trailEnds);
    }

    private readonly static int TrailEndHeight = 9;
}

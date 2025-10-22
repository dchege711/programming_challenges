namespace AoC2024;

public partial class HoofIt
{
    public int SumOfTrailHeadsScores()
    {
        var scores = new Dictionary<Coordinate, int>();
        foreach (var trailEnd in topographicMap.TrailEnds)
        {
            var toVisit = new HashSet<Coordinate>([trailEnd]);
            var visited = new HashSet<Coordinate>();
            while (toVisit.Count > 0)
            {
                var coordinate = toVisit.First();
                toVisit.Remove(coordinate);
                visited.Add(coordinate);
                if (topographicMap.Map[coordinate.r, coordinate.c] == TrailHeadHeight)
                {
                    if (scores.TryGetValue(coordinate, out var numReachable))
                        scores[coordinate] = numReachable + 1;
                    else
                        scores[coordinate] = 1;
                    continue;
                }

                PossibleMoves(coordinate)
                    .Where(newCoord => !visited.Contains(newCoord))
                    .ToList()
                    .ForEach(newCoord => toVisit.Add(newCoord));
            }
        }
        return scores.Values.Sum();
    }

    private IEnumerable<Coordinate> PossibleMoves(Coordinate coordinate)
    {
        var (r, c) = coordinate;
        int targetHeight = topographicMap.Map[coordinate.r, coordinate.c] - 1;
        return Deltas
            .Select(d => new Coordinate(r + d.dr, c + d.dc))
            .Where(IsInBounds)
            .Where(newCoord => topographicMap.Map[newCoord.r, newCoord.c] == targetHeight);
    }

    private static readonly List<(int dr, int dc)> Deltas = [
        (-1, 0), (1, 0), (0, -1), (0, 1)];

    private bool IsInBounds(Coordinate coordinate)
    {
        int rowCount = topographicMap.Map.GetLength(0);
        int colCount = topographicMap.Map.GetLength(1);
        var (r, c) = coordinate;
        return r >= 0 && r < rowCount && c >= 0 && c < colCount;
    }

    private readonly static int TrailHeadHeight = 0;
}

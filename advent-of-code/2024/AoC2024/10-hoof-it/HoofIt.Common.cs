namespace AoC2024;

public partial class HoofIt
{
    private IEnumerable<DistinctPaths> DistinctCompleteTrails(Coordinate trailHead)
    {
        var numPaths = new Dictionary<Coordinate, int>{{trailHead, 1}};
        var reachableTrailEnds = new HashSet<Coordinate>();
        var toVisit = new HashSet<Coordinate>([trailHead]);

        while (toVisit.Count > 0)
        {
            var current = toVisit.First();
            toVisit.Remove(current);

            if (topographicMap.Map[current.r, current.c] == TrailEndHeight)
            {
                reachableTrailEnds.Add(current);
                continue;
            }
            
            var currNumPaths = numPaths[current];
            PossibleMoves(current)
                .ToList()
                .ForEach(next =>
                {
                    var numOldPaths = numPaths.GetValueOrDefault(next, 0);
                    numPaths[next] = numOldPaths + currNumPaths;
                    toVisit.Add(next);
                });

        }

        var val = reachableTrailEnds
            .Select(coordinate => new DistinctPaths(coordinate, numPaths[coordinate]));
        return val;
    }

    private IEnumerable<Coordinate> PossibleMoves(Coordinate coordinate)
    {
        var (r, c) = coordinate;
        int targetHeight = topographicMap.Map[coordinate.r, coordinate.c] + 1;
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

    private readonly static int TrailEndHeight = 9;

    private record struct DistinctPaths(Coordinate Coordinate, int NumDistinctPaths);
}
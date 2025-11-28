namespace AoC2024;

public partial class HoofIt
{    
    public int SumOfTrailHeadsScores() =>
        topographicMap
            .TrailHeads
            .Select(DistinctCompleteTrails)
            .Select(x => x.Count())
            .Sum();
    
    private IEnumerable<DistinctPaths> DistinctCompleteTrails(Coordinate trailHead)
    {
        var numPaths = new Dictionary<Coordinate, int>{{trailHead, 1}};
        var reachableTrailEnds = new HashSet<Coordinate>();
        var toVisit = new HashSet<Coordinate>([trailHead]);

        while (toVisit.Count > 0)
        {
            var coordinate = toVisit.First();
            var numPathsToCoordinate = numPaths[coordinate];
            toVisit.Remove(coordinate);

            if (topographicMap.Map[coordinate.r, coordinate.c] == TrailEndHeight)
            {
                reachableTrailEnds.Add(coordinate);
                continue;
            }
            
            PossibleMoves(coordinate)
                .ToList()
                .ForEach(newCoord =>
                {
                    var numOldPaths = numPaths.GetValueOrDefault(newCoord, 0);
                    numPaths[newCoord] = numOldPaths + numPathsToCoordinate;
                    toVisit.Add(newCoord);
                });

        }

        return reachableTrailEnds
            .Select(coordinate => new DistinctPaths(coordinate, numPaths[coordinate]));
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

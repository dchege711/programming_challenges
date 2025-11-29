namespace AoC2024;

public partial class HoofIt
{
    private IEnumerable<DistinctPaths> DistinctCompleteTrails(Coordinate trailHead)
    {
        Queue<IReadOnlyList<Coordinate>> paths = [];
        paths.Enqueue([trailHead]);

        IReadOnlyList<IReadOnlyList<Coordinate>> trails = [];
        while (paths.Count > 0)
        {
            var path = paths.Dequeue();
            var current = path[path.Count - 1];

            if (topographicMap.Map[current.r, current.c] == TrailEndHeight)
            {
                trails = [..trails, path];
                continue;
            }
            
            PossibleMoves(current)
                .ToList()
                .ForEach(next => paths.Enqueue([..path, next]));
        }

        return trails
            .GroupBy(trail => trail[trail.Count - 1])
            .Select(g => new DistinctPaths(g.Key, g.Count()));
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
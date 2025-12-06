using System.Diagnostics;

namespace AoC2024;

public partial class GardenGroups
{
    private IEnumerable<Region> ComputeRegions()
    {
        bool[,] visited = new bool[garden.RowCount, garden.ColCount];
        for (int r = 0; r < garden.RowCount; r++)
        {
            for (int c = 0; c < garden.ColCount; c++)
            {
                if (visited[r, c])
                    continue;
                
                yield return GetRegion(new(r, c), visited);
            }
        }
    }

    private Region GetRegion(Coordinate start, bool[,] visited)
    {
        Debug.Assert(!visited[start.R, start.C]);

        Queue<Coordinate> toVisit = [];
        toVisit.Enqueue(start);

        var plantType = garden.PlantMap[start.R, start.C];
        var (area, perimeter, vertices) = (0, 0, 0);
        while (toVisit.Count > 0)
        {
            var (r, c) = toVisit.Dequeue();

            // Some cells e.g., the bottom right one in a rectangle have more
            // than one neighbor. Short-circuit.
            if (visited[r, c])
                continue;

            visited[r, c] = true;
            area += 1;

            var neighboringPlots = NavigationDeltas()
                .Select(d => new Coordinate(r + d.R, c + d.C))
                .Where(IsInBounds)
                .Where(coord => garden.PlantMap[coord.R, coord.C] == plantType);
            
            var perimeterContribution = 4 - neighboringPlots.Count();
            perimeter += perimeterContribution;
            if (perimeterContribution > 0)
                vertices += CountVertices(r, c);
            
            var unvisitedNeighboringPlots = neighboringPlots
                .Where(coord => !visited[coord.R, coord.C]);
            foreach (var coord in unvisitedNeighboringPlots)
                toVisit.Enqueue(coord);
        }
    
        return new(plantType, perimeter, area, vertices);
    }

    private static IReadOnlyList<Coordinate> NavigationDeltas() =>
        [new(1, 0), new(-1, 0), new(0, 1), new(0, -1)];
    
    private bool IsInBounds(Coordinate coordinate) =>
        IsInBounds(coordinate.R, coordinate.C);
    
    private bool IsInBounds(int r, int c) =>
        r >= 0 && r < garden.RowCount && c >= 0 && c < garden.ColCount;
    
    private int CountVertices(int r, int c)
    {
        var matchStates = GetEightDirectionalNeighborsMatchState(r, c).ToArray();
        Debug.Assert(matchStates.Length == 8);
        var (nw, n, ne, w, e, sw, s, se) = (
            matchStates[0], matchStates[1], matchStates[2], matchStates[3],
            matchStates[4], matchStates[5], matchStates[6], matchStates[7]);
        IReadOnlyList<bool> cornerTests = [
            !n && !w, // NW corner is an outward facing region corner
            !w && nw && n, // NW corner is an inward facing region corner
            !n && !e, // NE corner is an outward facing region corner
            !e && n && ne, // NE corner is an inward facing region corner
            !s && !e, // SE corner is an outward facing region corner
            !e && s && se, // SE corner is an inward facing region corner
            !s && !w, // SW corner is an outward facing region corner
            !w && s && sw, // SW corner is an inward facing region corner
        ];
        var count = cornerTests.Count(b => b);
        Debug.Assert(count <= 4);
        return count;
    }

    private IEnumerable<bool> GetEightDirectionalNeighborsMatchState(int r, int c)
    {
        var targetPlantType = garden.PlantMap[r, c];
        foreach (var dR in Enumerable.Range(-1, 3))
        {
            foreach (var dC in Enumerable.Range(-1, 3))
            {
                if (dR == 0 && dC == 0)
                    continue;
                
                var (nR, nC) = (r + dR, c + dC);
                yield return IsInBounds(nR, nC) && garden.PlantMap[nR, nC] == targetPlantType;
            }
        }
    }

    internal record struct Coordinate(int R, int C);

    internal record struct Region(
        char PlantType,
        int Perimeter,
        int Area,
        int VerticesCount);
}
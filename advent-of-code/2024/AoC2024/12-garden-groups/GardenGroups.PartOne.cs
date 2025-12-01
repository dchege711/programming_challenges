using System.Collections.Immutable;
using System.Diagnostics;

namespace AoC2024;

public partial class GardenGroups
{
    public int ComputeTotalFencingPrice() =>
        ComputeRegions()
            .Select(region => region.Area * region.Perimeter)
            .Sum();

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
        var (area, perimeter) = (0, 0);
        while (toVisit.Count > 0)
        {
            var (r, c) = toVisit.Dequeue();

            // Some cells e.g., the bottom right one in a rectangle have more
            // than one neighbor. Short-circuit.
            if (visited[r, c])
                continue;

            visited[r, c] = true;
            area += 1;

            var neighboringPlots = Deltas()
                .Select(d => new Coordinate(r + d.R, c + d.C))
                .Where(IsInBounds)
                .Where(coord => garden.PlantMap[coord.R, coord.C] == plantType);
            
            perimeter += 4 - neighboringPlots.Count();
            
            var unvisitedNeighboringPlots = neighboringPlots
                .Where(coord => !visited[coord.R, coord.C]);
            foreach (var coord in unvisitedNeighboringPlots)
                toVisit.Enqueue(coord);
        }
    
        return new(plantType, perimeter, area);
    }

    private static IReadOnlyList<Coordinate> Deltas() =>
        [new(1, 0), new(-1, 0), new(0, 1), new(0, -1)];
    
    private bool IsInBounds(Coordinate coordinate) =>
        coordinate.R >= 0 && coordinate.R < garden.RowCount
        && coordinate.C >= 0 && coordinate.C < garden.ColCount;

    internal record struct Coordinate(int R, int C);

    internal record struct RowBounds(int Left, int Right);
    internal record struct Region(char PlantType, int Perimeter, int Area);
}
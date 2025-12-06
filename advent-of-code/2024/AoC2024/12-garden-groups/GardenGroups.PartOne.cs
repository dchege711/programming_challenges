using System.Collections.Frozen;
using System.Collections.Immutable;
using System.Diagnostics;

namespace AoC2024;

public partial class GardenGroups
{
    public int ComputeTotalFencingPrice() =>
        ComputeRegions()
            .Sum(region => region.Area * region.Perimeter);

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
        var vertices = ImmutableList.CreateBuilder<Coordinate>();
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
                vertices.AddRange(GetVertices(new(r, c), neighboringPlots));
            
            var unvisitedNeighboringPlots = neighboringPlots
                .Where(coord => !visited[coord.R, coord.C]);
            foreach (var coord in unvisitedNeighboringPlots)
                toVisit.Enqueue(coord);
        }
    
        return new(plantType, perimeter, area, vertices.ToImmutableList());
    }

    private static IReadOnlyList<Coordinate> NavigationDeltas() =>
        [new(1, 0), new(-1, 0), new(0, 1), new(0, -1)];
    
    private bool IsInBounds(Coordinate coordinate) =>
        coordinate.R >= 0 && coordinate.R < garden.RowCount
        && coordinate.C >= 0 && coordinate.C < garden.ColCount;
    
    private static IEnumerable<Coordinate> GetVertices(Coordinate cell, IEnumerable<Coordinate> neighboringCells)
    {
        var neighbors = neighboringCells.ToFrozenSet();
        var (r, c) = cell;
        return VerticesTests()
            .Where(v => v.DisqualifyingNeighboringDeltas.All(d => !neighbors.Contains(new(r + d.R, c + d.C))))
            .Select(v => new Coordinate(r + v.VertexDelta.R, c + v.VertexDelta.C));
    }

    private static IReadOnlyList<VertexTest> VerticesTests() =>
        [
            new(new(0, 0), [new(-1, 0), new(0, -1)]),
            new(new(0, 1), [new(0, 1), new(-1, 0)]),
            new(new(1, 1), [new(0, 1), new(1, 0)]),
            new(new(1, 0), [new(1, 0), new(0, -1)])
        ];

    internal record struct Coordinate(int R, int C);

    internal record struct RowBounds(int Left, int Right);

    internal record struct VertexTest(
        Coordinate VertexDelta,
        IReadOnlyList<Coordinate> DisqualifyingNeighboringDeltas);

    internal record struct Region(
        char PlantType,
        int Perimeter,
        int Area,
        IReadOnlyList<Coordinate> Vertices);
}
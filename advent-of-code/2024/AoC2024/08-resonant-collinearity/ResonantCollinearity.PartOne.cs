namespace AoC2024;

public partial class ResonantCollinearity
{
    public int PartOne() => NumDistinctAntinodes(CollinearPairedAntinodes);

    private int NumDistinctAntinodes(
        Func<Coordinate, Coordinate, IEnumerable<Coordinate>> computeAntinodes)
    {
        HashSet<(Coordinate, Coordinate)> computedPairs = [];
        HashSet<Coordinate> antinodeLocations = [];

        foreach (var coordinates in antennasMap.AntennasByFrequency.Values)
        {
            for (int i = 0; i < coordinates.Count; i++)
            {
                for (int j = i + 1; j < coordinates.Count; j++)
                {
                    var (p1, p2) = (coordinates[i], coordinates[j]);
                    if (computedPairs.Contains((p1, p2)))
                        continue;
                    
                    foreach (var antinode in computeAntinodes(p1, p2))
                        antinodeLocations.Add(antinode);
                    
                    computedPairs.Add((p1, p2));
                }
            }
        }

        return antinodeLocations.Count;
    }

    private IEnumerable<Coordinate> CollinearPairedAntinodes(Coordinate p1, Coordinate p2)
    {
        var (dr, dc) = (p2.R - p1.R, p2.C - p1.C);

        return new Coordinate[]
        {
            new(p2.R + dr, p2.C + dc),
            new(p1.R - dr, p1.C - dc)
        }.Where(InBounds);
    }

    private bool InBounds(Coordinate p) =>
        p.R >= 0 && p.R < antennasMap.RowCount
        && p.C >= 0 && p.C < antennasMap.ColCount;
}

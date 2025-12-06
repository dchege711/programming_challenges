namespace AoC2024;

public partial class GardenGroups
{
    public int ComputeTotalDiscountedFencingPrice() =>
        ComputeRegions()
            .Sum(region => region.Area * ComputeNumberOfSides(region));

    private static int ComputeNumberOfSides(Region region) =>
        region.Vertices
            .GroupBy(v => v.R)
            .Concat(region.Vertices.GroupBy(v => v.C))
            .Sum(g => g.Count() - 1);
}
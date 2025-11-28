namespace AoC2024;

public partial class HoofIt
{
    public int SumOfTrailHeadsRatings() =>
        topographicMap
            .TrailHeads
            .Select(DistinctCompleteTrails)
            .SelectMany(x => x.Select(y => y.NumDistinctPaths))
            .Sum();
}

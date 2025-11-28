namespace AoC2024;

public partial class HoofIt
{
    public int SumOfTrailHeadsRatings() =>
        topographicMap
            .TrailHeads
            .Select(DistinctCompleteTrails)
            .SelectMany(completeTrails => completeTrails.Select(t => t.NumDistinctPaths))
            .Sum();
}

namespace AoC2024;

public partial class HoofIt
{    
    public int SumOfTrailHeadsScores() =>
        topographicMap
            .TrailHeads
            .Select(DistinctCompleteTrails)
            .Select(completeTrails => completeTrails.Count())
            .Sum();
}

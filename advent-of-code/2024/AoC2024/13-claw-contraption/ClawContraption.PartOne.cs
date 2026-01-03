namespace AoC2024;

public partial class ClawContraption
{
    public static long PartOne(string filePath) =>
        Parse(filePath)
            .Select(GetMinimumCost)
            .Where(cost => cost != long.MaxValue)
            .Sum();
}
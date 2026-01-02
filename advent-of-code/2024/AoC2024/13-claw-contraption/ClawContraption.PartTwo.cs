namespace AoC2024;

public partial class ClawContraption
{
    public static int PartTwo(string filePath) =>
        Parse(filePath)
            .Select(config => new MachineConfig(config.A, config.B, AddOffset(config.Prize)))
            .Select(GetMinimumCost)
            .Where(cost => cost != int.MaxValue)
            .Sum();
    
    private static readonly ulong offset = 10_000_000_000_000;

    private static Vector AddOffset(Vector v) => new(v.X + offset, v.Y + offset);
}
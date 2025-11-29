namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static IEnumerable<ulong> ReadStones(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        return inputReader.ReadLine()?.Split().Select(ulong.Parse) ?? [];
    }
}
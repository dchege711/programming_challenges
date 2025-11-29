namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static ulong NumStonesAfterBlinks(IEnumerable<ulong> stones, int numBlinks)
    {
        var sum = 0UL;
        foreach (var count in stones.Select(stone => GetNumChildStones(stone, 1, numBlinks)))
            sum += count;
        return sum;
    }

    public static IEnumerable<ulong> Blink(ulong stone)
    {
        if (stone == 0)
            return [1];
        
        var numDigits = GetNumberOfDigits(stone);
        if (numDigits % 2 == 0)
        {
            var newLength = numDigits / 2;
            var pow10 = (ulong)(Math.Pow(10, newLength));
            var a = stone / pow10;
            return [a, stone - a * pow10];
        }

        return [stone * 2024UL];
    }

    private static ulong GetNumChildStones(ulong stone, int iteration, int numBlinks)
    {
        if (iteration > numBlinks) return 1;

        var numChildren = 0UL;
        foreach (var child in Blink(stone))
            numChildren += GetNumChildStones(child, iteration + 1, numBlinks);

        return numChildren;
    }

    private static int GetNumberOfDigits(ulong n) => $"{n}".Length;
}
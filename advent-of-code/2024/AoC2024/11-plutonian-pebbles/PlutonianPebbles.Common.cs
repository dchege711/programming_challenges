namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static ulong NumStonesAfterBlinks(IEnumerable<ulong> stones, int numBlinks)
    {
        var sum = 0UL;
        var cache = new Dictionary<(ulong, int), ulong>();
        foreach (var count in stones.Select(stone => GetNumChildStones(stone, numBlinks, cache)))
            sum += count;
        return sum;
    }

    public static IReadOnlyList<ulong> Blink(ulong stone)
    {
        if (stone == 0)
            return [1];
        
        var numDigits = GetNumberOfDigits(stone);
        if (numDigits % 2 == 0)
        {
            var newLength = numDigits / 2;
            var pow10 = (ulong)Math.Pow(10, newLength);
            var a = stone / pow10;
            return [a, stone - a * pow10];
        }

        return [stone * 2024UL];
    }

    private static ulong GetNumChildStones(
        ulong stone, int blinksRemaining, Dictionary<(ulong, int), ulong> cache)
    {
        var subProblem = (stone, blinksRemaining);
        if (cache.TryGetValue(subProblem, out var numChildStones))
            return numChildStones;

        if (blinksRemaining <= 0) return 1;

        foreach (var child in Blink(stone))
            numChildStones += GetNumChildStones(child, blinksRemaining - 1, cache);
        
        cache[subProblem] = numChildStones;
        return numChildStones;
    }

    private static int GetNumberOfDigits(ulong n) => n switch
    {
        0 => 1,
        _ => (int)Math.Floor(Math.Log10(n)) + 1,
    };
}
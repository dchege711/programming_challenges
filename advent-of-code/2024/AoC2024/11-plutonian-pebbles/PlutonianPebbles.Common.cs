namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static ulong NumStonesAfterBlinks(IEnumerable<ulong> stones, int numBlinks)
    {
        var sum = 0UL;
        var cache = new Dictionary<(ulong, int), ulong>();
        foreach (var count in stones.Select(stone => GetNumChildStones(stone, 1, numBlinks, cache)))
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
            var pow10 = (ulong)Math.Pow(10, newLength);
            var a = stone / pow10;
            return [a, stone - a * pow10];
        }

        return [stone * 2024UL];
    }

    private static ulong GetNumChildStones(
        ulong stone,
        int iteration,
        int numBlinks,
        Dictionary<(ulong, int), ulong> cache)
    {
        if (cache.TryGetValue((stone, iteration), out var numChildren))
            return numChildren;

        if (iteration > numBlinks) return 1;

        foreach (var child in Blink(stone))
            numChildren += GetNumChildStones(child, iteration + 1, numBlinks, cache);
        
        cache[new(stone, iteration)] = numChildren;
        return numChildren;
    }

    private static int GetNumberOfDigits(ulong n) => n switch
    {
        0 => 1,
        _ => (int)Math.Floor(Math.Log10(n)) + 1,
    };
}
namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static long NumStonesAfter25Blinks(IEnumerable<ulong> stones) => 0;

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

    private static int GetNumberOfDigits(ulong n) => $"{n}".Length;
}
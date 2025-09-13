using System.Collections.Immutable;
using System.Text.RegularExpressions;

namespace AoC2024;

public partial class BridgeRepair
{
    public static IEnumerable<CalibrationEquation> Parse(string filePath)
    {
        using var inputReader = new StreamReader(filePath);
        Regex inputLineRegex = NumbersRegex();

        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            var numbers = inputLineRegex.Matches(line)
                .Select(match => int.Parse(match.Value));
            yield return new(numbers.First(), numbers.Skip(1).ToImmutableList());
        }
    }

    public readonly record struct CalibrationEquation(
        int Result, ImmutableList<int> Operands);

    [GeneratedRegex(@"\d+")]
    static private partial Regex NumbersRegex();
}

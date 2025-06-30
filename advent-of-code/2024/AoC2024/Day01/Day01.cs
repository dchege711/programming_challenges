using System.Text.RegularExpressions;

namespace AoC2024.Day01;

internal partial class Solution
{
    internal static void PartOne()
    {
        string baseFileName = "sample";
        // When you use the positional syntax for property definition, the
        // compiler creates a `Deconstruct` method with an `out` parameter for
        // each positional parameter provided in the record declaration. The
        // method deconstructs properties defined by using positional syntax;
        // it ignores properties that are defined by using standard property
        // syntax.
        //
        // [1]: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/record#positional-syntax-for-property-and-field-definition
        var (right, left, expectedTotalDistance) = ParseLocationIds(baseFileName);
        right = from id in right
                orderby id descending
                select id;
        left = from id in left
               orderby id descending
               select id;
        var totalDistance = left.Zip(right, (x1, x2) => int.Abs(x1 - x2)).Sum();

        if (expectedTotalDistance is int expectedVal)
        {
            if (totalDistance != expectedVal)
            {
                throw new InvalidOperationException(
                    $"Expected {expectedVal} for {baseFileName}.in.txt; instead got {totalDistance}");
            }
            else
            {
                Console.WriteLine($"Successfully computed {totalDistance} for {baseFileName}.in.txt");
            }
        }
        else
        {
            Console.WriteLine($"Total distance for {baseFileName}.in.txt is {totalDistance}");
        }

    }

    private static LocationIdsAndDistance ParseLocationIds(string baseFileName)
    {
        List<int> left = [];
        List<int> right = [];

        using StreamReader inputReader = new($"Day01/data/{baseFileName}.in.txt");
        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            Match match = InputLineRegex().Match(line);
            if (!match.Success)
                throw new ArgumentException($"{line} is not well formatted");

            left.Add(int.Parse(match.Groups["left"].Value));
            right.Add(int.Parse(match.Groups["right"].Value));
        }

        var expectedDistanceFilePath = $"Day01/data/{baseFileName}.ans.txt";
        if (!File.Exists(expectedDistanceFilePath))
            return new(left, right, null);

        using StreamReader outputReader = new(expectedDistanceFilePath);
        line = outputReader.ReadLine();
        if (line is null)
            throw new ArgumentException($"Unexpected null from {expectedDistanceFilePath}");

        return new(left, right, int.Parse(line));
    }

    private readonly record struct LocationIdsAndDistance(
        IEnumerable<int> Left, IEnumerable<int> Right, int? ExpectedTotalDistance);

    // Using source generation provides the most efficient code.
    //
    // `RegexOptions.Compiled` improves on bare `new Regex()`, e.g., "match the
    // input character at the current position against 'a' or 'c'" vs. "match
    // the input character at the current position against the set specified in
    // this set description"
    // 
    // However, `RegexOptions.Compiled` is costly to construct and uses
    // reflection. `GeneratedRegex` sidesteps these limitations while still
    // being efficient.
    //
    // [1]: https://learn.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-source-generators
    [GeneratedRegex(@"(?<left>\d+)\s+(?<right>\d+)")]
    private static partial Regex InputLineRegex();
}
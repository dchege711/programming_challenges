using System.Text.RegularExpressions;

namespace AoC2024.Day01;

// Creating a static class is basically the same as creating a class that
// contains only static members and a private constructor. A private constructor
// prevents the class from being instantiated. The advantage of using a static
// class is that the compiler can check to make sure that no instance members
// are accidentally added. The compiler guarantees that instances of this class
// can't be created.
//
// Static classes are sealed and therefore can't be inherited. They can't
// inherit from any class or interface except Object. Static classes can't
// contain an instance constructor. However, they can contain a static
// constructor. Non-static classes should also define a static constructor if
// the class contains static members that require non-trivial initialization.
//
// [1]: https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/static-classes-and-static-class-members
internal static partial class Solution
{
    internal static void PartOne()
    {
        string inputBaseName = "test-01";
        // When you use the positional syntax for property definition, the
        // compiler creates a `Deconstruct` method with an `out` parameter for
        // each positional parameter provided in the record declaration. The
        // method deconstructs properties defined by using positional syntax;
        // it ignores properties that are defined by using standard property
        // syntax.
        //
        // [1]: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/record#positional-syntax-for-property-and-field-definition
        var (left, right, expectedTotalDistance) = ParseLocationIds(inputBaseName, "pt1");
        left = from id in left
               orderby id ascending
               select id;
        right = from id in right
                orderby id ascending
                select id;
        var totalDistance = left.Zip(right, (x1, x2) => int.Abs(x1 - x2)).Sum();
        ReportExecution(inputBaseName, totalDistance, expectedTotalDistance);
    }

    internal static void PartTwo()
    {
        string inputBaseName = "test-01";
        var (left, right, expectedSimilarity) = ParseLocationIds(inputBaseName, "pt2");

        // Objective: Use a functional approach. Avoid mutating values.
        var rightGroupedIds = from id in right
                              group id by id;
        var rightLookupTable = new Dictionary<int, int>(
            from g in rightGroupedIds
            select new KeyValuePair<int, int>(g.Key, g.Count())
        );
        var similarityScore = (
            from id in left
            select id * rightLookupTable.GetValueOrDefault(id, 0)
        ).Sum();
        ReportExecution(inputBaseName, similarityScore, expectedSimilarity);
    }

    private static void ReportExecution(string inputBaseName, int actual, int? expected)
    {
        if (expected is int expectedVal)
        {
            if (actual != expectedVal)
            {
                throw new InvalidOperationException(
                    $"Expected {expectedVal} for {inputBaseName}.in.txt; instead got {actual}");
            }
            else
            {
                Console.WriteLine($"Successfully computed {actual} for {inputBaseName}.in.txt");
            }
        }
        else
        {
            Console.WriteLine($"Computed value for {inputBaseName}.in.txt is {actual}");
        }
    }

    private static LocationIdsAndExpectedValue ParseLocationIds(
        string inputBaseName,
        string outputSuffix)
    {
        List<int> left = [];
        List<int> right = [];

        using StreamReader inputReader = new($"Day01/data/{inputBaseName}.in.txt");
        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            Match match = InputLineRegex().Match(line);
            if (!match.Success)
                throw new ArgumentException($"{line} is not well formatted");

            left.Add(int.Parse(match.Groups["left"].Value));
            right.Add(int.Parse(match.Groups["right"].Value));
        }

        var expectedDistanceFilePath = $"Day01/data/{inputBaseName}.{outputSuffix}.ans.txt";
        if (!File.Exists(expectedDistanceFilePath))
            return new(left, right, null);

        using StreamReader outputReader = new(expectedDistanceFilePath);
        line = outputReader.ReadLine();
        if (line is null)
            throw new ArgumentException($"Unexpected null from {expectedDistanceFilePath}");

        return new(left, right, int.Parse(line));
    }

    private readonly record struct LocationIdsAndExpectedValue(
        IEnumerable<int> Left, IEnumerable<int> Right, int? Expected);

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
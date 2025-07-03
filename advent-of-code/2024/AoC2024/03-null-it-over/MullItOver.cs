using System.Text.RegularExpressions;

namespace AoC2024;

public static partial class MullItOver
{
    public static int PartOne(string filePath)
    {
        return ParseCommands(filePath).Select(cmd => cmd.Num1 * cmd.Num2).Sum();
    }

    private static IEnumerable<MultiplyCommand> ParseCommands(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;
        while((line = inputReader.ReadLine()) != null)
        {
            var commands = InputLineRegex()
                .Matches(line)
                .Select(m => (m.Groups["num1"].Value, m.Groups["num2"].Value))
                .Select(m => new MultiplyCommand(int.Parse(m.Item1), int.Parse(m.Item2)));
            
            foreach (var command in commands)
                yield return command;
        }
    }

    private record MultiplyCommand(int Num1, int Num2);

    [GeneratedRegex(@"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)")]
    private static partial Regex InputLineRegex();
}
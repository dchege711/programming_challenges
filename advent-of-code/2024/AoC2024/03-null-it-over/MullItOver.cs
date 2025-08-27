using System.Text.RegularExpressions;

namespace AoC2024;

public static partial class MullItOver
{
    public static int PartOne(string filePath)
    {
        return ParseCommands(filePath)
            .OfType<MultiplyCommand>()
            .Select(cmd => cmd.Num1 * cmd.Num2)
            .Sum();
    }

    public static int PartTwo(string filePath)
    {
        return ParseCommands(filePath).Aggregate(
            (true, 0), 
            (res, cmd) => cmd switch {
                DoCommand doCmd => (true, res.Item2),
                DontCommand dontCmd => (false, res.Item2),
                MultiplyCommand multCmd => (res.Item1, res.Item1 ? res.Item2 + (multCmd.Num1 * multCmd.Num2) : res.Item2),
                _ => throw new ArgumentException($"Unrecognized type {cmd.GetType()}")
            },
            res => res.Item2);
    }

    private static IEnumerable<ICommand> ParseCommands(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;
        while((line = inputReader.ReadLine()) != null)
        {
            var commands = InputLineRegex()
                .Matches(line)
                .Select(ParseCommand);
            
            foreach (var command in commands)
                yield return command;
        }
    }

    private static ICommand ParseCommand(Match m)
    {
        var groups = m.Groups;

        if (groups["do"].Success)
        {
            return new DoCommand();
        }
        else if (groups["dont"].Success)
        {
            return new DontCommand();
        }
        else if (groups["mul"].Success)
        {
            return new MultiplyCommand(
                int.Parse(groups["num1"].Value),
                int.Parse(groups["num2"].Value));
        }

        throw new ArgumentException($"Unrecognized match found {m}");
    }

    private interface ICommand;

    private record MultiplyCommand(int Num1, int Num2) : ICommand;
    private record DoCommand : ICommand;
    private record DontCommand : ICommand;

    [GeneratedRegex(@"(?<dont>don't\(\))|(?<do>do\(\))|(?<mul>mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\))")]
    private static partial Regex InputLineRegex();
}

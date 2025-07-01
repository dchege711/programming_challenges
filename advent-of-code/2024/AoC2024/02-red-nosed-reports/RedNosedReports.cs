namespace AoC2024;

public static class RedNosedReports
{
    public static int PartOne(string filePath)
    {
        var reports = GetReports(filePath);
        var numValidLevels = 0;
        foreach (var levels in reports)
        {
            var isValidLevel = true;
            Monotonicity? levelMonotonicity = null;
            var prevLevel = levels.First();
            foreach (var level in levels.Skip(1))
            {
                levelMonotonicity ??= GetMonotonicityIfValid(prevLevel, level);
                if (levelMonotonicity is null)
                {
                    isValidLevel = false;
                    break;
                }
                else if (GetMonotonicityIfValid(prevLevel, level) != levelMonotonicity)
                {
                    isValidLevel = false;
                    break;
                }
                
                prevLevel = level;
            }

            numValidLevels += isValidLevel ? 1 : 0;
        }
        return numValidLevels;
    }

    private static Monotonicity? GetMonotonicityIfValid(int a, int b) =>
        (b - a) switch
        {
            (>= 1) and (<= 3) => Monotonicity.Increasing,
            (>= -3) and (<= -1) => Monotonicity.Decreasing,
            _ => null,
        };

    private static IEnumerable<IEnumerable<int>> GetReports(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            yield return line.Split().Select(int.Parse);
        }
    }

    private enum Monotonicity { Increasing, Decreasing };
}
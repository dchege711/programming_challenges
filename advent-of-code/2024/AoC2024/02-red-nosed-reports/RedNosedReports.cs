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
                var monotonicity = GetMonotonicityIfValid(prevLevel, level, levelMonotonicity);
                levelMonotonicity ??= monotonicity;
                if (monotonicity is null)
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

    public static int PartTwo(string filePath)
    {
        var reports = GetReports(filePath);
        var numValidLevels = 0;
        
        foreach (var levels in reports)
        {
            var isValidLevel = true;
            var canDampenBadLevel = true;
            Monotonicity? levelMonotonicity = null;

            // Need multiple random accesses. A concrete list is useful to avoid
            // re-enumeration.
            var levelsArray = levels.ToArray();
            for (int i = 1; i < levelsArray.Length; i++)
            {
                var (prevLevel, level) = (levelsArray[i-1], levelsArray[i]);

                // Happy case: no skips needed.
                var monotonicity = GetMonotonicityIfValid(prevLevel, level, levelMonotonicity);
                if (monotonicity is not null)
                {
                    levelMonotonicity ??= monotonicity;
                    continue;
                }

                // Can a skip help us? The 3rd item can change the target
                // monotonicity.
                Monotonicity? monotonicityWithSkip = null;
                if (i - 2 >= 0)
                {
                    monotonicityWithSkip = GetMonotonicityIfValid(
                        levelsArray[i-2],
                        level,
                        i == 2 ? null : levelMonotonicity);
                }

                if (canDampenBadLevel && monotonicityWithSkip is not null)
                {
                    canDampenBadLevel = false;
                    levelMonotonicity ??= monotonicityWithSkip;
                    continue;
                }

                // If we're at the last entry and can ignore a bad level, do so.
                if (i == levelsArray.Length - 1)
                    continue;

                isValidLevel = false;
                break;
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
    
    private static Monotonicity? GetMonotonicityIfValid(
        int a,
        int b,
        Monotonicity? targetMonotonicity)
    {
        var monotonicity = GetMonotonicityIfValid(a, b);
        if (targetMonotonicity is null)
            return monotonicity;
        else if (monotonicity == targetMonotonicity)
            return monotonicity;
        else
            return null;
    }

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
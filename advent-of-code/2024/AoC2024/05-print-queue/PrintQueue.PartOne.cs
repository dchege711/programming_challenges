namespace AoC2024;

public partial class PrintQueue
{
    public int PartOne() =>
        printJobs
            .Where(IsValidJob)
            .Select(job => job[job.Count / 2])
            .Sum();

    private bool IsValidJob(IEnumerable<int> job) =>
        job.SelectMany((p1, idx) => job.Skip(idx + 1)
                .Select(p2 => IsValidOrderedPair(p1, p2)))
            .All(valid => valid);

    private bool IsValidOrderedPair(int p1, int p2) =>
        orderingRules.TryGetValue(p1, out var rules) && rules.Contains(p2);
}

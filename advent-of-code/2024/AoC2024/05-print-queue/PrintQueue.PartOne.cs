namespace AoC2024;

public partial class PrintQueue
{
    public int PartOne() =>
        printJobs
            .Where(IsValidJob)
            .Select(job => job[job.Count / 2])
            .Sum();

    private bool IsValidJob(IEnumerable<int> job) =>
        job.Index()
            .SelectMany(idxAndP1 => job.Skip(idxAndP1.Index + 1)
                .Select(p2 => IsValidOrderedPair(idxAndP1.Item, p2)))
            .All(valid => valid);

    private bool IsValidOrderedPair(int p1, int p2) =>
        orderingRules.TryGetValue(p1, out var rules) && rules.Contains(p2);
}

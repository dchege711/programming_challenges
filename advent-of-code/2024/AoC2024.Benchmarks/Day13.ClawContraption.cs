using BenchmarkDotNet.Attributes;

namespace AoC2024.Benchmarks;

[MemoryDiagnoser]
public class Day13ClawContraption
{    
    [Benchmark]
    public long PartOneBenchmark() =>
        ClawContraption.PartOne(BenchmarkUtils.GetResourcePath("day-13-test.in.txt"));
}
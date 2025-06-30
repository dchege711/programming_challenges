using AoC2024.Day01;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-01-sample.in.txt")]
[DeploymentItem("data/day-01-test.in.txt")]
public sealed class Day01Tests
{
    [TestMethod]
    [DataRow("day-01-sample.in.txt", 11)]
    [DataRow("day-01-test.in.txt", 1941353)]
    public void Part01(string filePath, int expected)
    {
        var locationIds = Day01.Solution.ParseLocationIds(filePath);
        var actual = Day01.Solution.PartOne(locationIds);
        actual.Should().Be(expected);
    }

    [TestMethod]
    [DataRow("day-01-sample.in.txt", 31)]
    [DataRow("day-01-test.in.txt", 22539317)]
    public void Part02(string filePath, int expected)
    {
        var locationIds = Day01.Solution.ParseLocationIds(filePath);
        var actual = Day01.Solution.PartTwo(locationIds);
        actual.Should().Be(expected);
    }
}

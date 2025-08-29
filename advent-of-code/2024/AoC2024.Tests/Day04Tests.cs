using AoC2024;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-04-sample.in.txt")]
[DeploymentItem("data/day-04-test.in.txt")]
public sealed class Day04CeresSearchTests
{
    [TestMethod]
    [DataRow("day-04-sample.in.txt", 18)]
    public void PartOne(string filePath, int expectedNumOccurrences)
    {
        var words = CeresSearch.ParseWordSearch(filePath);
        words.Count().Should().Be(10);
        words.Last().Should().Be("MXMXAXMASX");
    }
}

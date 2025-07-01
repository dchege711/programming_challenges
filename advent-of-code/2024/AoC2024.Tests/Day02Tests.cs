using AoC2024;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-02-sample.in.txt")]
[DeploymentItem("data/day-02-test.in.txt")]
public sealed class Day02RedNosedReports
{
    [TestMethod]
    [DataRow("day-02-sample.in.txt", 2)]
    [DataRow("day-02-test.in.txt", 486)]
    public void RedNosedReports01(string filePath, int expected)
    {
        RedNosedReports.PartOne(filePath).Should().Be(expected);
    }

    [TestMethod]
    [DataRow("day-02-sample.in.txt", 5)]
    [DataRow("day-02-test.in.txt", 540)]
    public void RedNosedReports02(string filePath, int expected)
    {
        RedNosedReports.PartTwo(filePath).Should().Be(expected);
    }
}
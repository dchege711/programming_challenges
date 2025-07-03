using AoC2024;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-03-sample.in.txt")]
[DeploymentItem("data/day-03-test.in.txt")]
public sealed class Day03MullItOverTests
{
    [TestMethod]
    [DataRow("day-03-sample.in.txt", 161)]
    [DataRow("day-03-test.in.txt", 183788984)]
    public void PartOne(string filePath, int expectedProduct)
    {
        MullItOver.PartOne(filePath).Should().Be(expectedProduct);
    }
}
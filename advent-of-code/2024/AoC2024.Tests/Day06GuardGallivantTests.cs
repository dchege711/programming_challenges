namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-06-sample.in.txt")]
[DeploymentItem("data/day-06-test.in.txt")]
public sealed class Day06GuardGallivantTests
{
    [TestMethod]
    public void Parsing()
    {
        var guardGallivant = new GuardGallivant("day-06-sample.in.txt");

        guardGallivant.areaMap.GetLength(0).Should().Be(10);
        guardGallivant.areaMap.GetLength(1).Should().Be(10);

        guardGallivant.areaMap[3, 2].Should().Be(GuardGallivant.PositionState.kBlocked);
        guardGallivant.areaMap[9, 9].Should().Be(GuardGallivant.PositionState.kUnVisited);

        guardGallivant.areaMap[6, 4].Should().Be(GuardGallivant.PositionState.kVisited);
        guardGallivant.startingPosition.Should().Be((6, 4, -1, 0));
    }

    [TestMethod]
    [DataRow("day-06-sample.in.txt", 41)]
    public void PartOne(string filePath, int expectedNumDistinctPositions)
    {
        var guardGallivant = new GuardGallivant(filePath);
        guardGallivant.PartOne().Should().Be(expectedNumDistinctPositions);
    }
}

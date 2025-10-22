namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-10-sample.in.txt")]
[DeploymentItem("data/day-10-sample-02.in.txt")]
[DeploymentItem("data/day-10-test.in.txt")]
public sealed class Day10HoofItTests
{
    [TestMethod]
    public void Parse()
    {
        var topographicMap = new HoofIt("day-10-sample.in.txt").topographicMap;

        int[,] expectedMap =
        {
            {0, 1, 2, 3},
            {1, 2, 3, 4},
            {8, 7, 6, 5},
            {9, 8, 7, 6}
        };
        topographicMap.Map.Should().BeEquivalentTo(expectedMap);
        topographicMap.TrailEnds.Should().BeEquivalentTo([
            new HoofIt.Coordinate(3, 0)
        ]);
    }

    [TestMethod]
    [DataRow("day-10-sample.in.txt", 1)]
    [DataRow("day-10-sample-02.in.txt", 36)]
    [DataRow("day-10-test.in.txt", 574)]
    public void PartOne(string filePath, int expectedSum)
    {
        var hoofIt = new HoofIt(filePath);
        hoofIt.SumOfTrailHeadsScores().Should().Be(expectedSum);
    }
}

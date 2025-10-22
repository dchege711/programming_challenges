namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-10-sample.in.txt")]
[DeploymentItem("data/day-10-sample-02.in.txt")]
public sealed class Day10HoofItTests
{
    [TestMethod]
    public void Parse()
    {
        var topographicMap = HoofIt.Parse("day-10-sample.in.txt");

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
}

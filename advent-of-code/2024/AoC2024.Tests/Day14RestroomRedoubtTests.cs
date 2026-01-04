using AoC2024.RestroomRedoubtDataTypes;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-14-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-14-test.in.txt")]
public sealed class Day14RestroomRedoubtTests
{
    [TestMethod]
    public void Parse()
    {
        var robots = RestroomRedoubt.Parse("day-14-sample.in.txt").ToList();
        robots.Count.Should().Be(12);
        robots[0].Should().Be(new Robot(new(0, 4), new(3, -3)));
        robots[4].Should().Be(new Robot(new(0, 0), new(1, 3)));
    }

    [TestMethod]
    [Ignore]
    [DataRow("day-14-sample.in.txt", 11, 7, 12)]
    public void PartOne(
        string filePath, int areaWidth, int areaHeight, int expectedSafetyFactor)
    {
        var safetyFactor = RestroomRedoubt.PartOne(filePath, areaWidth, areaHeight);
        safetyFactor.Should().Be(expectedSafetyFactor);
    }
}
using System.Collections.Immutable;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-07-sample.in.txt")]
[DeploymentItem("data/day-07-test.in.txt")]
public sealed class Day07BridgeRepairTests
{
    [TestMethod]
    public void Parse()
    {
        var calibrations = BridgeRepair.Parse("day-07-sample.in.txt");

        calibrations.Count().Should().Be(9);

        calibrations.First().Should().BeEquivalentTo(
            new BridgeRepair.CalibrationEquation(
                190, ImmutableList.Create([10, 19])));

        calibrations.Last().Should().BeEquivalentTo(
            new BridgeRepair.CalibrationEquation(
                292, ImmutableList.Create([11, 6, 16, 20])));
    }
}

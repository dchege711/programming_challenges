using Coordinate = AoC2024.ResonantCollinearity.Coordinate;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-08-sample.in.txt")]
[DeploymentItem("data/day-08-test.in.txt")]
public sealed class Day08ResonantCollinearityTests
{
    [TestMethod]
    public void Parse()
    {
        var antennasMap = ResonantCollinearity.Parse("day-08-sample.in.txt");

        antennasMap.RowCount.Should().Be(12);
        antennasMap.ColCount.Should().Be(12);
        antennasMap.AntennasByFrequency.Count.Should().Be(2);

        antennasMap.AntennasByFrequency['0'].Should().BeEquivalentTo(
            new List<Coordinate>([new(1, 8), new(2, 5), new(3, 7), new(4, 4)]));

        antennasMap.AntennasByFrequency['A'].Should().BeEquivalentTo(
            new List<Coordinate>([new(5, 6), new(8, 8), new(9, 9)]));
    }
}

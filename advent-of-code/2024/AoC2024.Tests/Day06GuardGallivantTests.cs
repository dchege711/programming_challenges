using System.Collections;

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

        var position = guardGallivant.areaMap[3, 2];
        position.Blocked.Should().BeTrue();
        position.Visits.HasAnySet().Should().BeFalse();

        position = guardGallivant.areaMap[9, 9];
        position.Blocked.Should().BeFalse();
        position.Visits.HasAnySet().Should().BeFalse();

        position = guardGallivant.areaMap[6, 4];
        position.Blocked.Should().BeFalse();
        position.Visits.HasAnySet().Should().BeTrue();
        guardGallivant.startingPosition.Should().Be((6, 4));
    }

    [TestMethod]
    [DataRow("day-06-sample.in.txt", 41)]
    [DataRow("day-06-test.in.txt", 5101)]
    public void PartOne(string filePath, int expectedNumDistinctPositions)
    {
        var guardGallivant = new GuardGallivant(filePath);
        guardGallivant.PartOne().Should().Be(expectedNumDistinctPositions);
    }

    [TestMethod]
    [DataRow("day-06-sample.in.txt", 6)]
    public void PartTwo(string filePath, int expectedNumDistinctPositions)
    {
        var guardGallivant = new GuardGallivant(filePath);
        guardGallivant.PartTwo().Should().Be(expectedNumDistinctPositions);
    }

    private static void PrintDebugString(GuardGallivant guardGallivant)
    {
        for (int r = 0; r < guardGallivant.areaMap.GetLength(0); r++)
        {
            for (int c = 0; c < guardGallivant.areaMap.GetLength(1); c++)
            {
                var (blocked, visits) = guardGallivant.areaMap[r, c];
                Console.Write(ToDebugString(blocked, visits));
            }
            Console.WriteLine();
        }
    }

    private static char ToDebugString(bool blocked, BitArray? visits)
    {
        if (blocked)
            return '#';
        else if (visits?.HasAnySet() == true)
            return 'X';
        return '.';
    }
}

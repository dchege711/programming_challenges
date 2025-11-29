namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-11-sample.in.txt")]
[DeploymentItem("data/day-11-test.in.txt")]
public sealed class Day11PlutonianPebblesTests
{
    [TestMethod]
    public void Parse()
    {
        var stones = PlutonianPebbles.ReadStones("day-11-sample.in.txt");
        stones.Should().BeEquivalentTo([125, 17]);
    }

    public static IEnumerable<(ulong stone, IEnumerable<ulong> expectedStones)> TestBlinkData
    {
        get
        {
            return [
                (0, [1]),
                (123, [123 * 2024]),
                (1234, [12, 34]),
                (10, [1, 0]),
                (1000, [10, 0]),
                (100005, [100, 5]),
            ];
        }
    }

    [TestMethod]
    [DynamicData(nameof(TestBlinkData))]
    public void Blink(ulong stone, IEnumerable<ulong> expectedStones) =>
        PlutonianPebbles.Blink(stone).Should().BeEquivalentTo(expectedStones);

    [TestMethod]
    [DataRow("day-11-sample.in.txt", 55312L)]
    [Ignore]
    public void PartOne(string fileName, long expectedNumStones)
    {
        var stones = PlutonianPebbles.ReadStones(fileName);
        var numStones = PlutonianPebbles.NumStonesAfter25Blinks(stones);
        numStones.Should().Be(expectedNumStones);
    }
}
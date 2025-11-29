namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-11-sample.in.txt")]
public sealed class Day11PlutonianPebblesTests
{
    [TestMethod]
    public void Parse()
    {
        var stones = PlutonianPebbles.ReadStones("day-11-sample.in.txt");
        stones.Should().BeEquivalentTo([125, 17]);
    }
}
namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-09-sample.in.txt")]
[DeploymentItem("data/day-09-test.in.txt")]
public sealed class Day09DiskFragmenterTests
{
    [TestMethod]
    public void Parse()
    {
        var diskMap = DiskFragmenter.Parse("day-09-sample.in.txt");
        diskMap.ToArray().Should().BeEquivalentTo([
            2, 3, 3, 3, 1, 3, 3, 1, 2, 1, 4, 1, 4, 1, 3, 1, 4, 0, 2]);
    }
}

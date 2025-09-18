namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/day-09-sample.in.txt")]
[DeploymentItem("data/day-09-sample-2.in.txt")]
[DeploymentItem("data/day-09-sample-3.in.txt")]
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

    [TestMethod]
    [DataRow("day-09-sample.in.txt", 1928)]
    [DataRow("day-09-sample-2.in.txt", 60)]
    [DataRow("day-09-sample-3.in.txt", 12)]
    public void PartOne(string filePath, long expectedChecksum)
    {
        var diskMap = DiskFragmenter.Parse(filePath);
        var checksum = DiskFragmenter.PartOne(diskMap);
        checksum.Should().Be(expectedChecksum);
    }
}

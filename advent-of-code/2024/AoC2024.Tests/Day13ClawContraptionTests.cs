namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-13-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-13-test.in.txt")]
public sealed class Day13ClawContraptionTests
{
    [TestMethod]
    public void Parse()
    {
        var machineConfigs = ClawContraption.Parse("day-13-sample.in.txt").ToArray();
        machineConfigs.Length.Should().Be(4);

        var sampleConfig = machineConfigs[1];
        sampleConfig.A.Delta.X.Should().Be(26);
        sampleConfig.A.Delta.Y.Should().Be(66);
        sampleConfig.B.Delta.X.Should().Be(67);
        sampleConfig.B.Delta.Y.Should().Be(21);
        sampleConfig.Prize.X.Should().Be(12748);
        sampleConfig.Prize.Y.Should().Be(12176);
    }
}
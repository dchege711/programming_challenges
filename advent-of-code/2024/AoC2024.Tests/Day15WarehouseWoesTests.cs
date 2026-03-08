using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-15-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-15-sample-2.in.txt")]
[DeploymentItem("data/scratchpad/day-15-sample-3.in.txt")]
[DeploymentItem("data/scratchpad/day-15-sample-5.in.txt")]
[DeploymentItem("data/scratchpad/day-15-test.in.txt")]
public sealed class Day15WarehouseWoesTests
{
    [TestMethod]
    public void ParseGrid()
    {
        var warehouseWoes = new WarehouseWoes("day-15-sample.in.txt", false);

        warehouseWoes.RobotPosition.Should().BeEquivalentTo(new Coordinate(4, 4));

        warehouseWoes.Grid.GetLength(0).Should().Be(10);
        warehouseWoes.Grid.GetLength(1).Should().Be(10);

        warehouseWoes.Grid[0, 0].Should().Be(CellType.Wall);
        warehouseWoes.Grid[4, 4].Should().Be(CellType.Free);
        warehouseWoes.Grid[5, 1].Should().Be(CellType.Box);
    }

    [TestMethod]
    public void ParseWideGrid()
    {
        var warehouseWoes = new WarehouseWoes("day-15-sample.in.txt", true);

        warehouseWoes.RobotPosition.Should().BeEquivalentTo(new Coordinate(4, 8));

        warehouseWoes.Grid.GetLength(0).Should().Be(10);
        warehouseWoes.Grid.GetLength(1).Should().Be(20);

        warehouseWoes.Grid[1, 0].Should().Be(CellType.Wall);
        warehouseWoes.Grid[1, 1].Should().Be(CellType.Wall);
        warehouseWoes.Grid[4, 9].Should().Be(CellType.Free);
        warehouseWoes.Grid[5, 2].Should().Be(CellType.BoxStart);
        warehouseWoes.Grid[5, 3].Should().Be(CellType.BoxEnd);
    }

    [DataRow("day-15-sample.in.txt", false, 10092)]
    [DataRow("day-15-sample-2.in.txt", false, 2028)]
    [DataRow("day-15-test.in.txt", false, 1486930)]
    [DataRow("day-15-sample.in.txt", true, 9021)]
    [DataRow("day-15-sample-3.in.txt", true, 618)]
    [DataRow("day-15-test.in.txt", true, 1492011)]
    [TestMethod]
    public void SumGpsCoordinates(string filePath, bool isWideVersion, int expectedSum)
    {
        var warehouseWoes = new WarehouseWoes(filePath, isWideVersion);
        var sumGpsCoordinates = warehouseWoes.Solve();
        sumGpsCoordinates.Should().Be(expectedSum);
    }

    [TestMethod]
    public void ZigZagEdgeCase()
    {
        var warehouseWoes = new WarehouseWoes("day-15-sample-5.in.txt", false);
        warehouseWoes.Solve();

        warehouseWoes.RobotPosition.Should().Be(new Coordinate(5, 7));
        warehouseWoes.Grid[7, 5].Should().Be(CellType.BoxStart);
        warehouseWoes.Grid[7, 6].Should().Be(CellType.BoxEnd);
        warehouseWoes.Grid[6, 6].Should().Be(CellType.BoxStart);
        warehouseWoes.Grid[6, 7].Should().Be(CellType.BoxEnd);
        warehouseWoes.Grid[7, 7].Should().Be(CellType.BoxStart);
        warehouseWoes.Grid[7, 8].Should().Be(CellType.BoxEnd);
        warehouseWoes.Grid[8, 7].Should().Be(CellType.Free);
        warehouseWoes.Grid[8, 8].Should().Be(CellType.Free);
    }
}

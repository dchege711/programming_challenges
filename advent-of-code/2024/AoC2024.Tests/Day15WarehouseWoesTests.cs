using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-15-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-15-sample-2.in.txt")]
[DeploymentItem("data/scratchpad/day-15-sample-3.in.txt")]
[DeploymentItem("data/scratchpad/day-15-test.in.txt")]
public sealed class Day15WarehouseWoesTests
{
    [TestMethod]
    public void ParseGrid()
    {
        var warehouseWoes = new WarehouseWoes("day-15-sample.in.txt", false);

        warehouseWoes.robotPosition.Should().BeEquivalentTo(new Coordinate(4, 4));

        warehouseWoes.grid.GetLength(0).Should().Be(10);
        warehouseWoes.grid.GetLength(1).Should().Be(10);

        warehouseWoes.grid[0, 0].Should().Be(CellType.Wall);
        warehouseWoes.grid[4, 4].Should().Be(CellType.Free);
        warehouseWoes.grid[5, 1].Should().Be(CellType.Box);
    }

    [TestMethod]
    public void ParseWideGrid()
    {
        var warehouseWoes = new WarehouseWoes("day-15-sample.in.txt", true);

        warehouseWoes.robotPosition.Should().BeEquivalentTo(new Coordinate(4, 8));

        warehouseWoes.grid.GetLength(0).Should().Be(10);
        warehouseWoes.grid.GetLength(1).Should().Be(20);

        warehouseWoes.grid[1, 0].Should().Be(CellType.Wall);
        warehouseWoes.grid[1, 1].Should().Be(CellType.Wall);
        warehouseWoes.grid[4, 9].Should().Be(CellType.Free);
        warehouseWoes.grid[5, 2].Should().Be(CellType.BoxStart);
        warehouseWoes.grid[5, 3].Should().Be(CellType.BoxEnd);
    }

    [TestMethod]
    public void ParseMoves()
    {
        var moves = WarehouseWoes.ParseMoves("day-15-sample.in.txt").ToArray();
        moves.Length.Should().Be(700);
        moves.First().Should().Be(Direction.Left);
        moves.Last().Should().Be(Direction.Up);
    }

    [DataRow("day-15-sample.in.txt", false, 10092)]
    [DataRow("day-15-sample-2.in.txt", false, 2028)]
    // [DataRow("day-15-test.in.txt", false, 1486930)]
    [DataRow("day-15-sample.in.txt", true, 9021)]
    // [DataRow("day-15-sample-3.in.txt", true, 618)]
    // [DataRow("day-15-test.in.txt", true, 1486930)]
    [TestMethod]
    public void SumGpsCoordinates(string filePath, bool isWideVersion, int expectedSum)
    {
        var warehouseWoes = new WarehouseWoes(filePath, isWideVersion);
        warehouseWoes.SimulateRobotMoves();
        var sumGpsCoordinates = warehouseWoes.grid.SumBoxGpsCoordinates();
        sumGpsCoordinates.Should().Be(expectedSum);
    }

    [TestMethod]
    public void PartTwo(string filePath, int expectedSum)
    {
        var sumGpsCoordinates = WarehouseWoes.PartTwo(filePath);
        sumGpsCoordinates.Should().Be(expectedSum);
    }
}

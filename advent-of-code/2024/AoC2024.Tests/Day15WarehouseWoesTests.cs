using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-15-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-15-sample-2.in.txt")]
[DeploymentItem("data/scratchpad/day-15-test.in.txt")]
public sealed class Day15WarehouseWoesTests
{
    [TestMethod]
    public void ParseGrid()
    {
        var (grid, startingPosition) = WarehouseWoes.ParseGrid("day-15-sample.in.txt", false);

        startingPosition.Should().BeEquivalentTo(new Coordinate(4, 4));

        grid.GetLength(0).Should().Be(10);
        grid.GetLength(1).Should().Be(10);

        grid[0, 0].Should().Be(CellType.Wall);
        grid[4, 4].Should().Be(CellType.Free);
        grid[5, 1].Should().Be(CellType.Box);
    }

    [TestMethod]
    public void ParseWideGrid()
    {
        var (grid, startingPosition) = WarehouseWoes.ParseGrid("day-15-sample.in.txt", true);

        startingPosition.Should().BeEquivalentTo(new Coordinate(4, 8));

        grid.GetLength(0).Should().Be(10);
        grid.GetLength(1).Should().Be(20);

        grid[1, 0].Should().Be(CellType.Wall);
        grid[1, 1].Should().Be(CellType.Wall);
        grid[4, 9].Should().Be(CellType.Free);
        grid[5, 2].Should().Be(CellType.BoxStart);
        grid[5, 3].Should().Be(CellType.BoxEnd);
    }

    [TestMethod]
    public void ParseMoves()
    {
        var moves = WarehouseWoes.ParseMoves("day-15-sample.in.txt").ToArray();
        moves.Length.Should().Be(700);
        moves.First().Should().Be(Direction.Left);
        moves.Last().Should().Be(Direction.Up);
    }

    [DataRow("day-15-sample.in.txt", 10092)]
    [DataRow("day-15-sample-2.in.txt", 2028)]
    [DataRow("day-15-test.in.txt", 1486930)]
    [TestMethod]
    public void PartOne(string filePath, int expectedSum)
    {
        var sumGpsCoordinates = WarehouseWoes.PartOne(filePath);
        sumGpsCoordinates.Should().Be(expectedSum);
    }

    [DataRow("day-15-sample.in.txt", 9021)]
    [DataRow("day-15-test.in.txt", 1486930)]
    [TestMethod]
    public void PartTwo(string filePath, int expectedSum)
    {
        var sumGpsCoordinates = WarehouseWoes.PartTwo(filePath);
        sumGpsCoordinates.Should().Be(expectedSum);
    }
}

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
        var (grid, startingPosition) = WarehouseWoes.ParseGrid("day-15-sample.in.txt");

        startingPosition.Should().BeEquivalentTo(new Coordinate(4, 4));

        grid.GetLength(0).Should().Be(10);
        grid.GetLength(1).Should().Be(10);

        grid[0, 0].Should().Be(CellType.Wall);
        grid[4, 4].Should().Be(CellType.Free);
        grid[5, 1].Should().Be(CellType.Box);
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
    [TestMethod]
    public void PartOne(string filePath, int expectedSum)
    {
        var sumGpsCoordinates = WarehouseWoes.PartOne(filePath);
        sumGpsCoordinates.Should().Be(expectedSum);
    }
}

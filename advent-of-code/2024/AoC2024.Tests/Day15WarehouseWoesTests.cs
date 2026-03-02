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
        var grid = WarehouseWoes.ParseGrid("day-15-sample.in.txt", false);

        grid.RobotPosition.Should().BeEquivalentTo(new Coordinate(4, 4));

        grid.RowCount.Should().Be(10);
        grid.ColCount.Should().Be(10);
        grid.BoxWidth.Should().Be(1);

        grid.Walls.Should().Contain(new Coordinate(0, 0));
        grid.Walls.Should().NotContain(new Coordinate(4, 4));

        grid.Boxes.Should().Contain(new Coordinate(5, 1));
        grid.Boxes.Should().NotContain(new Coordinate(4, 4));
    }

    [TestMethod]
    public void ParseWideGrid()
    {
        var grid = WarehouseWoes.ParseGrid("day-15-sample.in.txt", true);

        grid.RobotPosition.Should().BeEquivalentTo(new Coordinate(4, 8));

        grid.RowCount.Should().Be(10);
        grid.ColCount.Should().Be(20);
        grid.BoxWidth.Should().Be(2);

        grid.Walls.Should().Contain(new Coordinate(1, 0));
        grid.Walls.Should().Contain(new Coordinate(1, 1));
        grid.Walls.Should().NotContain(new Coordinate(4, 9));

        grid.Boxes.Should().Contain(new Coordinate(5, 2));
        grid.Boxes.Should().NotContain(new Coordinate(4, 9));
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

    // [DataRow("day-15-sample.in.txt", 9021)]
    [DataRow("day-15-sample-3.in.txt", 618)]
    // [DataRow("day-15-test.in.txt", 1486930)]
    [TestMethod]
    public void PartTwo(string filePath, int expectedSum)
    {
        var sumGpsCoordinates = WarehouseWoes.PartTwo(filePath);
        sumGpsCoordinates.Should().Be(expectedSum);
    }
}

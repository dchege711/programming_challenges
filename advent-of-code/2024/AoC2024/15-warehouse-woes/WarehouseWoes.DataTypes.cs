namespace AoC2024.WarehouseWoesDataTypes;

public enum Direction { Up, Right, Down, Left }

public record struct Coordinate(int R, int C);

public record struct Delta(int dR, int dC);

public enum CellType { Wall, Box, Free, BoxStart, BoxEnd }

public record Grid(
    int RowCount,
    int ColCount,
    Coordinate RobotPosition,
    HashSet<Coordinate> Walls,
    HashSet<Coordinate> Boxes,
    int BoxWidth);

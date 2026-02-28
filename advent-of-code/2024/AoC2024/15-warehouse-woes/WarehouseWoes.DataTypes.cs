namespace AoC2024.WarehouseWoesDataTypes;

public enum Move { Up, Right, Down, Left }

public record struct Coordinate(int R, int C);

public enum CellType { Wall, Box, Free }

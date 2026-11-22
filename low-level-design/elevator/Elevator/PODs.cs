namespace Elevator;

public enum ElevatorState { GoingUp, GoingDown, Idle }

public record struct ElevatorRequest(Direction Direction, int Floor);

public enum Direction { Up, Down }

public record struct ElevatorStop(int Floor, ElevatorState ElevatorState);

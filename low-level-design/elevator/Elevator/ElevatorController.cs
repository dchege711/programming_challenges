namespace Elevator;

public class ElevatorController
{
    public ElevatorController(int floorCount, int elevatorCount)
    {
        if (floorCount <= 0 || elevatorCount <= 0)
        {
            throw new ArgumentException(
                $"Number of floors ({floorCount}) and number of elevators ({elevatorCount}) must be positive.");
        }
        _floorCount = floorCount;

        List<Elevator> elevators = new(elevatorCount);
        for (var i = 0; i < elevatorCount; i++)
        {
            elevators.Add(new(floorCount, OnElevatorStop));
        }
        _elevators = elevators;
    }

    public Elevator Request(Direction direction, int originalFloor)
    {
        if (originalFloor <= 0 || originalFloor > _floorCount)
        {
            throw new ArgumentException(
                $"Floor {originalFloor} is in the range [1, {_floorCount}]");
        }

        // Prefer an elevator that's already headed to `originalFloor`.
        Elevator? maybeElevator = _elevators.FirstOrDefault(e => e.IsOnTheWayTo(direction, originalFloor));
        maybeElevator ??= _elevators.FirstOrDefault(e => e.State == ElevatorState.Idle);
        maybeElevator ??= _elevators[0]; // Last resort. Round-robin this?

        if (maybeElevator is not Elevator elevator)
        {
            throw new InvalidOperationException("Could not find an elevator");
        }

        elevator.Request(new(direction, originalFloor));
        return elevator;
    }

    public void AdvanceTicks(uint ticks)
    {
        for (uint i = 0; i < ticks; i++)
        {
            foreach (Elevator elevator in _elevators)
            {
                elevator.Tick();
            }
        }
    }

    public IReadOnlyList<ElevatorStop> ServicedStops() => _servicedStops;

    private void OnElevatorStop(ElevatorStop stop) => _servicedStops.Add(stop);

    private readonly List<ElevatorStop> _servicedStops = [];

    private readonly int _floorCount;
    private readonly List<Elevator> _elevators;
}

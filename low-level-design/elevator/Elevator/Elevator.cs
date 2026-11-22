namespace Elevator;

public class Elevator(int floorCount, Action<ElevatorStop> onStopAtFloor)
{
    public ElevatorState State { get; private set; } = ElevatorState.Idle;

    public int CurrentFloor { get; private set; } = 1;

    public void Tick()
    {
        ElevatorRequest? maybeSatisfiedRequest = State switch
        {
            ElevatorState.GoingUp => new(Direction.Up, CurrentFloor),
            ElevatorState.GoingDown => new(Direction.Down, CurrentFloor),
            ElevatorState.Idle => null,
            _ => null
        };

        if (maybeSatisfiedRequest is ElevatorRequest request
            && _elevatorRequests.Remove(request))
        {
            onStopAtFloor(new ElevatorStop(request.Floor, State));
            return;
        }

        if (_elevatorRequests.Count == 0)
        {
            State = ElevatorState.Idle;
            return;
        }

        if (State == ElevatorState.Idle)
        {
            State = GetStateForAdvancingToNearestRequest();
            return;
        }

        var keepMovingInSameDirection = HasMoreStopsInCurrentDirection();
        if (keepMovingInSameDirection)
        {
            CurrentFloor = GetNextFloor();
            return;
        }

        State = State switch
        {
            ElevatorState.GoingUp => ElevatorState.GoingDown,
            ElevatorState.GoingDown => ElevatorState.GoingUp,
            ElevatorState.Idle => throw new InvalidOperationException($"{State} doesn't support reversing."),
            _ => throw new InvalidOperationException($"{State} is not a known enum."),
        };
    }

    public void Request(ElevatorRequest request)
    {
        if (request.Floor <= 0 || request.Floor > floorCount)
        {
            throw new ArgumentException(
                $"Floor {request.Floor} is in the range [1, {floorCount}]");
        }

        _ = _elevatorRequests.Add(request);
    }

    public bool IsOnTheWayTo(Direction direction, int floor)
    {
        if (ToElevatorState(direction) != State)
        {
            return false;
        }

        return direction switch
        {
            Direction.Up => floor > CurrentFloor,
            Direction.Down => floor < CurrentFloor,
            _ => throw new ArgumentException($"{direction} is not a known enum.")
        };
    }

    private static ElevatorState ToElevatorState(Direction direction) => direction switch
    {
        Direction.Up => ElevatorState.GoingUp,
        Direction.Down => ElevatorState.GoingDown,
        _ => throw new ArgumentException($"{direction} is not a known enum.")
    };

    private int GetNextFloor() => State switch
    {
        ElevatorState.GoingUp => Math.Min(floorCount, CurrentFloor + 1),
        ElevatorState.GoingDown => Math.Max(1, CurrentFloor - 1),
        ElevatorState.Idle => CurrentFloor,
        _ => throw new ArgumentException($"{State} is not a known enum.")
    };

    private bool HasMoreStopsInCurrentDirection() => State switch
    {
        ElevatorState.GoingUp => _elevatorRequests.Any(r => r.Floor > CurrentFloor),
        ElevatorState.GoingDown => _elevatorRequests.Any(r => r.Floor < CurrentFloor),
        ElevatorState.Idle => false,
        _ => false
    };

    private ElevatorState GetStateForAdvancingToNearestRequest()
    {
        ElevatorState candidateState = ElevatorState.Idle;
        var shortestDistance = int.MaxValue;
        foreach (ElevatorRequest request in _elevatorRequests)
        {
            var distance = Math.Abs(request.Floor - CurrentFloor);
            if (distance < shortestDistance)
            {
                shortestDistance = distance;
                candidateState = request.Floor > CurrentFloor
                    ? ElevatorState.GoingUp
                    : ElevatorState.GoingDown;
            }
        }

        return candidateState;
    }

    private readonly HashSet<ElevatorRequest> _elevatorRequests = [];
}

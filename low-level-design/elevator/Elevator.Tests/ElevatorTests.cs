using FluentAssertions;

namespace Elevator.Tests;

[TestClass]
public class ElevatorTests
{
    [TestMethod]
    public void HallCall_Succeeds()
    {
        var controller = new ElevatorController(10, 1);
        const int callingFloor = 2;
        controller.Request(Direction.Up, callingFloor);

        controller.AdvanceTicks(3);
        controller.ServicedStops().Should().BeEqualTo([
            new ElevatorStop(callingFloor, ElevatorState.GoingUp)
        ]);
    }

    [TestMethod]
    public void InCabCall_Succeeds()
    {
        var controller = new ElevatorController(10, 1);

        var callingFloor = 6;
        Elevator elevator = controller.Request(Direction.Up, callingFloor);

        controller.AdvanceTicks(7);
        controller.ServicedStops().Should().BeEqualTo([
            new ElevatorStop(callingFloor, ElevatorState.GoingUp)
        ]);

        var targetFloor = 2;
        elevator.Request(new(Direction.Down, targetFloor));

        controller.AdvanceTicks(7);
        controller.ServicedStops().Should().BeEqualTo([
            new ElevatorStop(callingFloor, ElevatorState.GoingUp),
            new ElevatorStop(targetFloor, ElevatorState.GoingDown)
        ]);
    }

    [TestMethod]
    public void HallCall_BiDirectional_Succeeds()
    {
        var controller = new ElevatorController(10, 1);
        var callingFloor = 6;
        controller.Request(Direction.Up, callingFloor);
        controller.Request(Direction.Down, callingFloor);
        controller.AdvanceTicks(10);

        controller.ServicedStops().Should().BeEqualTo([
            new ElevatorStop(callingFloor, ElevatorState.GoingUp),
            new ElevatorStop(callingFloor, ElevatorState.GoingDown)
        ]);
    }
}

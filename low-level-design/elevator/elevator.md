---
date: 2026-11-01
domains:
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/low-level-design/elevator/elevator/
title: 'Elevator Control System'
---

Design an elevator control system for a building. The system should handle
multiple elevators, floor requests, and move elevators efficiently to service
requests. {{% cite ElevatorHelloInterview %}}

## Requirements Gathering

Let there be \\(E\\) elevators serving \\(F\\) floors. Users can request an
up/down elevator from any of the \\(F\\) floors. Once inside an elevator, the
users can request any number of floors. The elevator stops at each of the
requested floors.

{{% comment %}}

{{% cite ElevatorHelloInterview %}} gives me more vocabulary for the above,
e.g., a "hall call" is when a user requests an elevator from one of the \\(F\\)
floors.

{{% /comment %}}

{{% comment %}}

This is not the only interaction model. Some buildings have destination dispatch
systems such that the user picks the floor that they want to go to, and then the
elevator system assigns them an elevator. {{% cite ElevatorHelloInterview %}}

{{% /comment %}}

We need a model of time in the system as that's how elevators transition from
one floor to the next.

{{% comment %}}

Choosing a simulation-based design is an important decision to run by the
interviewer. Real elevators interface with hardware, e.g., getting a callback
on reaching each floor. {{% cite ElevatorHelloInterview %}}

{{% /comment %}}

The following mechanics are out of scope:

* Reserving an elevator, e.g., for a move.
* Monitoring capacity and weight limits on the elevator.
* Emergency operation mode, e.g., during a fire.
* Putting some of the elevators in maintenance mode.
* Door mechanics, e.g., door being prevented from closing.

{{% comment %}}

Forgot to add UI rendering. {{% cite ElevatorHelloInterview %}}

It seems that most of these LLD design problems center on user-facing software,
and so scoping out the UI is a common thing to do. However, I should practice
what to do if the interview chooses that UI is in scope, because that's what
real systems would have.

{{% /comment %}}

## Core Entities and Relationships

The `ElevatorController` receives requests from the panels found in each floor,
and its main job is to assign an elevator.

```txt
enum Direction { Up, Down }

class ElevatorController:
  - floorCount: int
  - elevators: Elevator[]

  + Request(Direction direction, int callingFloor) -> Elevator
  + AdvanceTicks(uint ticks) -> void
```

The `Elevator` maintains state and needs to decide how to move in order to
satisfy all of the assigned stops. Assignments can come from
`ElevatorController`, or the panel inside the elevator, where users can specify
floors.

```txt
enum ElevatorState { GoingUp, GoingDown, Idle }

struct ElevatorRequest:
  - direction: Direction
  - floor: int

class Elevator:
  - state: ElevatorState
  - currentFloor: int
  - requests: HashSet<ElevatorRequests>

  + Request(ElevatorRequest) -> void
  + Tick() -> void
```

{{% comment %}}

{{% cite ElevatorHelloInterview %}} has `Elevator` distinguish between up/down
requests made in the hall, and floor requests made inside the elevator. I don't
see why an `Elevator` needs to care where the request came from. They'd need to
stop anyway.

{{% /comment %}}

## Implementation

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/elevator/Elevator/PODs.cs"
  highlight="csharp" >}}

While both `ElevatorController.Request` and `Elevator.Request` both seem to add
a request, their internals are wildly different. `ElevatorController.Request`
picks an elevator, while `Elevator.Request` mutates state. An interface like:

```cs
interface IRequestHandler
{
  void Handle(ElevatorRequest request);
}
```

... would be misplaced polymorphism. The controller is not a type of elevator.
{{% cite ElevatorHelloInterview %}}

`ElevatorController.Request` needs to pick an elevator to route the request to.
This piece is amenable to the [strategy pattern]({{< ref
"/computer-science/low-level-design/object-oriented-design-patterns/behavioral-patterns/strategy.md"
>}}), e.g.,

* Pick the elevator closest to the floor.
* Pick the elevator going in the same direction as the request, and moving
  towards the floor.
* Pick the elevator going in the same direction as the request, and whose queued
  requests will take it to/past the floor before reversing.

... where the extensibility makes sense if you're working for an elevator
company serving different buildings with different needs, e.g., prioritizing
wait time, prioritizing energy efficiency, etc. {{% cite ElevatorHelloInterview
%}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/elevator/Elevator/ElevatorController.cs"
  highlight="csharp" >}}

`Elevator.tick()` needs an algorithm for how to service stops. Evaluate them
based on desired user experience. Consider an elevator that starts at floor 5,
and receives requests for floors 8, 3, and 7:

* First in, first out. The elevator does \\(5 \to 8 \to 3 \to 7\\). The user at
  floor 7 sees the elevator go all the way to 8, then down to 3, before coming
  back to them at 7, making for a confusing experience.
* Always go to the nearest stop, regardless of direction. The elevator does \\(5
  \to 3 \to 7 \to 8\\). In a building with two busy floors, the elevator might
  bounce around the busy floors, leaving users on other floors waiting longer.
* SCAN: continue until clear, then reverse. Elevator does \\(5 \to 7 \to 8 \to
  3\\), with only one reversal. This also matches the experience in most
  buildings, and is intuitive. If the user sees the elevator coming to their
  floor, they know they'll get picked up.

{{% cite ElevatorHelloInterview %}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/elevator/Elevator/Elevator.cs"
  highlight="csharp" >}}

{{% comment %}}

Choosing what `Elevator.tick()` handles in the current tick vs. defers to the
next tick tripped me up. The sample implementation in {{% cite
ElevatorHelloInterview %}} seems to draw the lines somewhat arbitrarily. I guess
it's better to handle less in a tick, than handling more and missing out on some
edge case. In the end `Elevator.tick()` sets the `Elevator`'s state: which
direction are we moving, and which floor are we at. As long as the `tick()`
iteration mutates one, we'll eventually converge. Sure, we'll take more `tick()`
calls, but it's less bug-prone.

{{% /comment %}}

## Extensibility

### Support Express Elevators That Only Stops at Pre-Defined Major Floors

`ElevatorController.Request` needs to change from `Request(Direction direction,
int callingFloor) -> Elevator` to `Request(int callingFloor, int
destinationFloor) -> Elevator`. This allows `ElevatorController` to assign the
`Elevator` based on whether `destinationFloor` is pre-defined as an express
stop.

`void Elevator.Request(ElevatorRequest request)` also needs to reject
non-express floors if the `Elevator` is express. Even better, we can do away
with `Elevator.Request` and have `ElevatorController.Request` as the only way to
select a floor. This avoids the case of users being stuck in an elevator that
can't take them where they need to go.

### Support Cancelling a Floor Request

`Elevator` is modular such that cancelling a floor request is a matter of
removing it from its `HashSet<ElevatorRequests>`.

{{% comment %}}

IRL, cancelling a floor request is not supported because people in the hallway
already saw that a stop is queued for their floor, and thus didn't press the
button. {{% cite ElevatorHelloInterview %}}

{{% /comment %}}

### Support Multiple Hall Calls Coming in at the Same Time

Concurrent `ElevatorController.Request` calls might assign the same elevator,
concurrently modifying the `Elevator`'s `HashSet<ElevatorRequests>`.

{{% comment %}}

I initially assumed that because adding to a `HashSet` is idempotent, the only
case that matters is assigning the same `Elevator` a request that goes up, and a
request that goes down, leading to longer wait times. However, [don't confuse
idempotency for thread-safety]({{< ref
"/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/correctness-in-concurrency.md"
>}}). Idempotency and concurrency are two different concepts, and one doesn't
imply the other.

{{% /comment %}}

Furthermore, `ElevatorController.Request` might add to the `Elevator`'s
`HashSet<ElevatorRequests>`, while `Elevator.Tick` is iterating over the
`HashSet`. This invalidates the enumerator, causing an
`InvalidOperationException`. {{% cite ElevatorHelloInterview %}}

{{% comment %}}

Carefully consider what needs to be atomic. Controlling concurrency only at
`ElevatorController.Request` does not eliminate concurrency bugs from
`ElevatorController.Tick`'s overlapping execution.

{{% /comment %}}

Use the same lock in `Request` and `Tick` to avoid concurrent access. A hall
call waits while `Tick` is running, and vice-versa. {{% cite
ElevatorHelloInterview %}}

{{% open-comment %}}

{{% cite ElevatorHelloInterview %}}'s concurrency discussion doesn't explicitly
handle in-cab `Elevator.Request` calls leading to concurrency bugs. The `lock`
in `ElevatorController` does not prevent concurrent `HashSet` modifications
originating from `Elevator.Request`.

{{% /open-comment %}}

## References

1. {{< citation
  id="ElevatorHelloInterview"
  title="Elevator Low Level Design | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/problem-breakdowns/elevator"
  accessed="2026-11-01" >}}

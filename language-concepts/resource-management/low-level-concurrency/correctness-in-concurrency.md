---
date: 2026-10-22
domains:
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/correctness-in-concurrency/
title: Ensuring Correctness in Concurrent Execution
---

## Bug Patterns

### Check-Then-Act

Involves checking a condition and then making a decision based on that check,
e.g., in a ticket booking system, checking that a seat is available and then
booking it if so. {{% cite HelloInterviewCorrectness %}}

Consider this buggy code:

```cs
public class RateLimiterBroken
{
  private readonly Dictionary<string, int> _requestCounts = [];
  private readonly int _maxRequests = 100;

  public bool ShouldAllowRequest(string userId)
  {
    _requestCounts.TryGetValue(userId, out int count);

    if (count >= _maxRequests) { return false; }

    _requestCounts[userId] = count + 1;
    return true;
  }
}
```

... where thread A reads `count` as `99`, but before it updates the map, thread
B also reads `count` as `99`. Both threads A and B increment the count from `99`
to `100` even though the user has made `101` requests. {{% cite
HelloInterviewCorrectness %}}

### Read-Modify-Write

Involves reading a value, computing something from it, and writing the result
back. Bugs manifest as lost updates, e.g.,

```cs
public class RequestCounterBroken
{
  private int _requestCount = 0;

  // Increment operations are actually three steps (read, add, write) that can
  // interleave.
  public void OnRequest() => _requestCount++;
}
```

... where threads `A` and `B` both read `5`, and both increment to `6`, even
though the final count should be `7`. {{% cite HelloInterviewCorrectness %}}

## Solutions

### Atomic Variables

Atomic variables use special CPU instructions to perform read-modify-write
operations in a single, uninterruptible step without needing a lock, e.g.,

```cs
public class BookingStats
{
  private int _bookedCount = 0;

  public void OnSeatBooked => Interlocked.Increment(ref _bookedCount);

  public int GetBookedCount() => Interlocked.CompareExchange(ref _bookedCount, 0, 0);
}
```

{{% cite HelloInterviewCorrectness %}}

{{% comment %}}

`Interlocked.CompareExchange(ref location, newValue, expected)` returns the
original value. {{% cite HelloInterviewCorrectness %}}

{{% /comment %}}

Complex updates need a compare-and-swap (CAS) loop, e.g.,

```cs
public class ConcurrencyTracker
{
  private int _maxConcurrent = 0;

  public void UpdateMaxConcurrent(int current)
  {
    int observed;
    do
    {
      observed = _maxConcurrent;
      if (current <= observed) { return; }
    }
    while (Interlocked.CompareExchange(ref _maxConcurrent, current, observed) != observed);
  }
}
```

{{% cite HelloInterviewCorrectness %}}

{{% open-comment %}}

This is not intuitive code! Have there been ergonomic improvements since?

{{% /open-comment %}}

However, atomics only work for single variables. Atomics in a booking system
wouldn't work for booking multiple seats:

```cs
public class TicketBookingBuggyAtomic
{
  private readonly ConcurrentDictionary<string, string> _seatOwners = [];

  public bool BookSeat(string seatId, string visitorId) => _seatOwners.TryAdd(seatId, visitorId);

  // Cannot atomically update two separate atomic reference objects.
  public bool BookSeats(string seat1, string visitor1, string seat2, string visitor2) { ... }
}
```

{{% cite HelloInterviewCorrectness %}}

### Coarse-Grained Locking

The lock creates a critical section where only one thread executes at a time.
The check and the update happen together with no possibility of interleaving,
e.g.,

```cs
public class TicketBooking
{
  private readonly object _bookingLock = new object();
  private Dictionary<string, string> _seatOwners = [];

  public bool BookSeat(string seatId, string visitorId)
  {
    lock (_bookingLock)
    {
      if (_seatOwners.ContainsKey(seatId)) { return false; }

      _seatOwners[seatId] = visitorId;
      return true;
    }
  }
}
```

{{% cite HelloInterviewCorrectness %}}

Do not release the lock too early, e.g.,

```cs
public class TicketBooking
{
  private readonly object _bookingLock = new object();
  private Dictionary<string, string> _seatOwners = [];

  public bool BookSeat(string seatId, string visitorId)
  {
    bool isAlreadyTaken = false;
    lock (_bookingLock)
    {
      isAlreadyTaken = _seatOwners.ContainsKey(seatId);
    }

    if (isAlreadyTaken) { return false; }

    // Bug: A concurrent thread might give this seat to someone else!
    _seatOwners[seatId] = visitorId;
    return true;
  }
}
```

{{% cite HelloInterviewCorrectness %}}

Don't use different locks for operations that need to be atomic together, e.g.,

```cs
public class TicketBookingBroken2
{
  private readonly object _availabilityLock = new object();
  private readonly object _assignmentLock = new object();
  private Dictionary<string, string> _seatOwners = [];

  public bool IsAvailable(string seatId)
  {
    lock (_availabilityLock) { return !_seatOwners.ContainsKey(seatId); }
  }

  public void MarkBooked(string seatId, string visitorId)
  {
    lock (_assignmentLock) { _seatOwners[seatId] = visitorId; }
  }
}
```

{{% cite HelloInterviewCorrectness %}}

Coarse-grained locking is the right choice for short critical sections (think
milliseconds) and moderate contention. Human-triggered operations are in this
category, e.g., a ticketing system with 10,000 users racing for seats won't have
more than a few dozen hitting the lock at the exact same microsecond. {{% cite
HelloInterviewCorrectness %}}

### Read-Write (Shared-Exclusive) Locks

A workload might be heavily skewed towards reads. A coarse-grained lock would be
wasteful because readers block each other even though they're not modifying
anything.

```cs
public class Cache
{
  private readonly ReaderWriterLockSlim _rwLock = new();
  private readonly Dictionary<string, string> _data = [];

  public string? Get(string key)
  {
    _rwLock.EnterReadLock();

    // Multiple threads can hold the read lock simultaneously because they're
    // just reading and can't corrupt each other's view.

    try { return _data.TryGetValue(key, out var value) ? value : null; }
    finally { _rwLock.ExitReadLock(); }
  }

  public void Put(string key, string value)
  {
    _rwLock.EnterWriteLock();

    // The write lock is exclusive. It waits for all readers to finish, and then
    // blocks everyone else until the write completes.

    try { _data[key] = value; }
    finally { _rwLock.ExitWriteLock(); }
  }
}
```

{{% cite HelloInterviewCorrectness %}}

### Fine-Grained Locking

Different threads calling the same function acquire different locks based on the
inputs, e.g.,

```cs
public class TicketBookingFineGrained
{
  private readonly ConcurrentDictionary<string, object> _seatLocks = [];
  private readonly ConcurrentDictionary<string, string> _seatOwners = [];

  private object GetLock(string seatId) => _seatLocks.GetOrAdd(seatId, _ => new object());

  public bool BookSeat(string seatId, string visitorId)
  {
    lock (GetLock(seatId))
    {
      if (_seatOwners.ContainsKey(seatId)) { return false; }

      _seatOwners[seatId] = visitorId;
      return true;
    }
  }
}
```

... allowing for greater throughput, assuming the input is randomly distributed.
{{% cite HelloInterviewCorrectness %}}

Fine-grained locking incurs complexity when operations span multiple "splits",
e.g., Alice wants to swap seat `7A` for `12B`, while Bob wants to swap `12B` for
`7A`. This code will deadlock:

```cs
public class TicketBookingDeadlock
{
  public bool SwapSeats(string visitor1, string seat1, string visitor2, string seat2)
  {
    lock (GetLock(seat1))
    {
      lock (GetLock(seat2))
      {
        // ... perform swap.
        return true;
      }
    }
  }
}
```

... if thread `A` locks `7A` waiting for `12B`, and thread `B` locks `12B`
waiting for `7A`. {{% cite HelloInterviewCorrectness %}}

The solution is to acquire locks in a consistent order, e.g.,

```cs
bool shouldLockSeat1First = string.Compare(seat1, seat2) < 0;
string first = shouldLockSeat1First ? seat1 : seat2;
string second = shouldLockSeat1First ? seat2 : seat1;

lock (GetLock(first))
{
  lock (GetLock(second))
  {
    // ... perform swap
    return true;
  }
}
```

{{% cite HelloInterviewCorrectness %}}

Fine-grained locking also introduces practical overhead. A long running
application with unbounded resources would need to clean up locks that are no
longer in use. {{% cite HelloInterviewCorrectness %}}

### Thread Confinement (Shared Nothing)

Partition the dataso that each thread owns its slice, e.g., in a booking system,
have thread 1 handle sections `A-M`, thread 2 handle sections `N-Z`. Database
connection pools often give each thread its own connection. {{% cite
HelloInterviewCorrectness %}}

Thread confinement trades synchronization complexity for architectural
complexity, e.g., coordinating actions that span multiple partitions (booking
seats in sections `B` and `S`), load imbalances if some partitions are hotter
than others, etc. {{% cite HelloInterviewCorrectness %}}

## References

1. {{< citation
  id="HelloInterviewCorrectness"
  title="Correctness in Low Level Design | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/concurrency/correctness"
  accessed="2026-10-22" >}}

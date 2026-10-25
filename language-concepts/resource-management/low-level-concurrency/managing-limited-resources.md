---
date: 2026-10-24
domains:
- en.wikipedia.org
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/managing-limited-resources/
title: Managing Limited Resources
---

## Limit Concurrent Operations

Applies when you have operations that can run concurrently, but you need to cap
how many can run at once, e.g., integrating with an external API that allows at
most 10 concurrent requests per account, limiting number of image processing
jobs to avoid CPU saturation, etc. {{% cite ScarcityHelloInterview %}}

Semaphores solve this directly, e.g.,

```cs
public class APIClient
{
  // Limit to at most 5 concurrent requests.
  private readonly SemaphoreSlim _requestPermits = new SemaphoreSlim(5);

  public async Task<Response> MakeRequestAsync(string endpoint)
  {
    await _requestPermits.WaitAsync(); // Block if 5 requests are in-flight

    try { return await _httpClient.GetAsync(endpoint); }
    finally { _requestPermits.Release(); }
  }
}
```

{{% cite ScarcityHelloInterview %}}

## Limit Aggregate Consumption

Applies when the constraint isn't on the number of concurrent operations, but
how much of some resource those operations are collectively using, e.g.,
limiting data uploads to \\(N\\) MB to avoid overwhelming memory buffers. {{%
cite ScarcityHelloInterview %}}

Use a semaphore, where each permit represents a unit of the resource, e.g.,

```cs
public class DiskWriter
{
  private const int MB = 1024 * 1024;
  private readonly object _lock = new object();
  private int _availableMB = 100;

  public async Task WriteFileAsync(byte[] data, string path)
  {
    int neededMB = Math.Max(1, (data.length + MB - 1) / MB); // Round up permits

    lock (_lock)
    {
      while (_availableMB < neededMB) { Monitor.Wait(_lock); }
      _availableMB -= neededMB;
    }

    try { await File.WriteAllBytesAsync(path, data); }
    finally
    {
      lock (_lock)
      {
        _availableMB += neededMB;
        Monitor.PulseAll(_lock);
      }
    }
  }
}
```

{{% cite ScarcityHelloInterview %}}

## Reuse Expensive Objects

The constraint is a fixed set of expensive objects that are costly to create but
can be reused, e.g., database connections which take time to establish, GPU
contexts which consume memory, file handles which are limited by the OS. {{%
cite ScarcityHelloInterview %}}

```cs
public class ConnectionPool
{
  private readonly BlockingCollection<Connection> _availableConnections;

  public ConnectionPool(int poolSize)
  {
    _availableConnections = new BlockingCollection<Connection>(poolSize);

    for (int i = 0; i < poolSize; i++)
    {
      _availableConnections.Add(CreateNewConnection());
    }
  }

  public Connection Acquire() => return _availableConnections.Take();

  public void Release(Connection connection) => _availableConnections.Add(connection);

  public void ExecuteQuery(string query)
  {
    var connection = Acquire();

    try { connection.Execute(query); }
    finally { Release(connection); }
  }
}
```

{{% open-comment %}}

Can't `ConnectionPool` expose an `IDisposable` that returns the `Connection` to
the `BlockingCollection` on disposal? Every `Acquire` call needs a `Release`
call, and disposables seem like the C# idiom for achieving guaranteed cleanup.

{{% /open-comment %}}

Creating all the objects upfront slows down startup, but subsequent requests are
fast. Lazy initialization needs to take into account race conditions when
establishing the objects. {{% cite ScarcityHelloInterview %}}

Use timeout variants to avoid blocking forever on request paths, e.g.,

```cs
public class ConnectionPool
{
  // ...
  private readonly TimeSpan _timeout;

  public ConnectionPool(int poolSize, TimeSpan timeout) { ... }

  public Connection Acquire()
  {
    if (_availableConnections.TryTake(out var conn, _timeout)) { return conn; }

    throw new TimeoutException(
      $"No connection available within {_timeout.TotalMilliseconds}ms");
  }
}
```

{{% cite ScarcityHelloInterview %}}

Most blocking queue implementations are approximately FIFO in which thread gets
woken up. At the cost of reduced throughput, you can guarantee strict fairness,
e.g., using an `ArrayBlockingQueue` with `fair=true`. {{% cite
ScarcityHelloInterview %}}

Avoid nested checkout of objects as that may deadlock. Suppose there are 10
connections, and 10 worker threads, each using a connection. If these threads
try to check out a second connection, then they'll become stuck. Instead, have
each worker request all of its connections upfront as an all-or-nothing step.
{{% cite ScarcityHelloInterview %}}

`ExecuteQuery` can catch fatal network or protocol errors, and use that to
discard an invalid `Connection` and replace it with a valid one. This is useful
for long-lived objects that can be invalidated outside the pooling mechanism,
e.g., DB restarts, voiding current connections. {{% cite ScarcityHelloInterview
%}}

{{% comment %}}

IMO, it should be up to the `Connection` itself to ensure its own validity.
Having the `ConnectionPool` take that into account muddies the waters for me.

{{% /comment %}}

## How to Determine Capacity

Little's Law states that:

$$ L = \lambda W $$

... where \\(L\\) is the long-term average number of customers in a stationary
system, and \\(\lambda\\) is the long-term average effective arrival rate, and
\\(W\\) is the average time a customer spends in the system. In most queuing
systems, service time is the bottleneck that creates the queue. {{% cite
LittlesLawWiki %}}

If a service receives 150 req/sec, and each request holds a DB connection for an
average of 20ms, then the average concurrent connection demand is \\( 150 \times
0.02 = 3\\). To run at no more than 75% average utilization, use a pool of
\\(\frac{3}{0.75} = 4\\) connections. {{% cite ScarcityHelloInterview %}}

## References

1. {{< citation
  id="ScarcityHelloInterview"
  title="Scarcity in Low Level Design | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/concurrency/scarcity"
  accessed="2026-10-24" >}}

1. {{< citation
  id="LittlesLawWiki"
  title="Little's law - Wikipedia"
  url="https://en.wikipedia.org/wiki/Little%27s_law"
  accessed="2026-10-24" >}}

---
date: 2026-10-22
domains:
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/coordinating-work-between-threads/
title: Coordinating Work Between Threads
---

## The Problem

Some work should not be ran in the mainline as it takes too long, and makes your
mainline unresponsive, e.g., on user signup, sending a welcome email should be
done out of line lest your application cannot process new requests.
Conceptually, API handlers produce tasks, workers consume tasks, and something
sits in between to coordinate the handoff. {{% cite CoordinationHelloInterview
%}}

Three things need solving in this scenario:

* **Efficient waiting**: Consumers should sleep when there's no work, and wake
  up immediately when new work arrives.
* **Backpressure**: Producers should slow down when consumers can't keep up,
  preventing memory exhaustion which would take down the whole application.
* **Thread safety**: The coordination mechanism itself must handle concurrent
  access without corruption.

{{% cite CoordinationHelloInterview %}}

## Anti-Patterns to Avoid

If your worker does:

```cs
while (true)
{
  if (queue.Count > 0)
  {
    var task = queue.Dequeue();
    Execute(task);
  }
}
```

... then they are busy-waiting; spinning in a tight loop, burning CPU while
doing no useful work. {{% cite CoordinationHelloInterview %}}

Avoiding the busy-waiting with:

```cs
while (true)
{
  if (queue.Count > 0)
  {
    var task = queue.Dequeue();
    Execute(task);
  }
  else
  {
    Thread.Sleep(100);
  }
}
```

... introduces latency, e.g., a task that arrives 1ms after a worker goes to
sleep waits nearly 100ms before being processed. {{% cite
CoordinationHelloInterview %}}

## Shared State Coordination

### Wait/Notify (Condition Variables)

[Condition variables]({{< ref
"/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/sync-primitives-w-python-specifics#condition-variables"
>}}) solve the problem of "sleep when there's nothing to do, then wake up the
instant work arrives", e.g.,

```cs
// WaitNotify.cs
lock (lockObj)
{
  while (!ConditionIsMet()) // Re-check on waking up lest another thread beat us
  {
    Monitor.Wait(lockObj); // Release lock; sleep until notified
  }

  DoWork();
  Monitor.PulseAll(lockObj); // Wake all waiting threads
}
```

{{% cite CoordinationHelloInterview %}}

`Monitor.Pulse` wakes up one waiting thread, chosen arbitrarily. If there are
different kinds of threads waiting, then `Monitor.PulseAll` guarantees that the
correct thread will be woken up. Even better, use separate condition variables,
e.g., one for "queue not empty" that only consumers wait on, another for "queue
not full" that only producers wait on, etc. {{% cite CoordinationHelloInterview
%}}

### Blocking Queues

{{% comment %}}

Some languages, e.g., C# and Go, have channels that are modern versions of
blocking queues. See [Passing Data Asynchronously Between Producers and
Consumers (.NET)]({{< ref
"/computer-science/algorithms-and-data-structures/derived-data/stream-processing/dotnet-threading-channels/"
>}}) for more about .NET's `Channel<T>` data structure.

{{% /comment %}}

[Blocking queues]({{< ref
"/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/sync-primitives-w-python-specifics#blocking-queues"
>}}) abstract away the manual wait/notify pattern, e.g.,

```cs
using System.Collections.Concurrent;

public record EmailTask(string Recipient, string Template, string Data);

public class EmailService
{
  private readonly BlockingCollection<EmailTask> _emailQueue =
    new BlockingCollection<EmailTask>(boundedCapacity: 10000);

  // API handler (producer)
  public void Signup(string email, string name)
  {
    _userRepository.Save(email, name);
    _emailQueue.Add(new EmailTask(email, "welcome", name)); // Blocks when full.
  }

  // Worker thread (consumer)
  public void EmailWorker()
  {
    while (true)
    {
      var emailTask = _emailQueue.Take(); // Blocks when empty
      _emailClient.Send(emailTask.Recipient, emailTask.Template, emailTask.Data);
    }
  }
}
```

{{% cite CoordinationHelloInterview %}}

Bound `BlockingCollection`'s capacity to tradeoff between throughput and memory
load, e.g., if workers can handle 100 tasks/sec, and you want to absorb a 10s
spike, set a capacity of \\(100 \times 10 = 1,000\\). {{% cite
CoordinationHelloInterview %}}

When the queue fills up, there are three options. If slowdown is acceptable,
then block producers and have them wait. If you can't stall, return a `429 (Too
Many Requests)` with a `Retry-After`. For lossy workloads, shed the load and
log. {{% cite CoordinationHelloInterview %}}

There are three options for graceful shutdown of workers that are waiting for
tasks. Interrupting the worker thread makes it wake up and throw
`InterruptedException`, breaking out of its loop. Instead of blocking forever
with `queue.take()`, use `queue.poll(timeout)` to return `null` if no task shows
up within `timeout`, allowing your worker to check a shutdown flag periodically.
Alternatively, define a poison pill task that means "shut down"; when a worker
pulls this, it should exit its loop. {{% cite CoordinationHelloInterview %}}

## Message Passing Coordination Using Actors

An **actor** is an independent unit of computation with three properties: it has
a mailbox (queue of incoming messages), it processes messages one at a time, and
it can send messages to other actors, e.g.,

```cs
using System.Collections.Concurrent;

public abstract class Actor<T>
{
  private readonly BlockingCollection<T> _mailbox = new();
  private readonly CancellationTokenSource _cts = new();

  protected Actor()
  {
    Task.Run(() => Run(_cts.Token));
  }

  public void Send(T message) => _mailbox.Add(message);

  protected abstract void OnReceive(T message);

  public void Stop()
  {
    _cts.Cancel();
    _mailbox.CompleteAdding();
  }

  private void Run(CancellationToken cancellationToken)
  {
    try
    {
      foreach (var message in _mailbox.GetConsumingEnumerable(cancellationToken))
      {
        OnReceive(message); // OnReceive never runs concurrently with itself.
      }
    }
    catch (OperationCanceledException) { }
  }
}

public class EmailActor : Actor<EmailRequest>
{
  private readonly EmailClient _emailClient = new();

  protected override void OnReceive(EmailRequest emailRequest) =>
    _emailClient.Send(emailRequest.To, emailRequest.Subject, emailRequest.Body);
}

public class SignupHandler
{
  private readonly EmailActor _emailActor = new();
  private readonly UserRepository _userRepository;

  public void HandleSignup(SignupRequest signupRequest)
  {
    var user = _userRepository.Save(new User(signupRequest.Email));
    _emailActor.Send(
      new EmailRequest(user.Email, "Welcome!", "Thanks for signing up..."));
  }
}
```

{{% cite CoordinationHelloInterview %}}

{{% open-comment %}}

See [Akka.NET](https://getakka.net/) for a production-grade treatment of the
actor model.

{{% /open-comment %}}

Actors are useful when you have many independent entities that occasionally
need to communicate, e.g, a trading system where each order book is an actor.
Because actors don't share state, they can be distributed across machines,
allowing for efficient scaling. {{% cite CoordinationHelloInterview %}}

Global ordering across senders is complicated. While messages from actor `A` to
actor `B` arrive in order, if `A` and `C` both send to `B`, then the
interleaving is undefined. {{% cite CoordinationHelloInterview %}}

Actors communicate asynchronously. To send a message and wait for a reply
requires some machinery, e.g., send a message with a callback address and wait
for the response. {{% cite CoordinationHelloInterview %}}

## References

1. {{< citation
  id="CoordinationHelloInterview"
  title="Coordination in Low Level Design | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/concurrency/coordination"
  accessed="2026-10-22" >}}

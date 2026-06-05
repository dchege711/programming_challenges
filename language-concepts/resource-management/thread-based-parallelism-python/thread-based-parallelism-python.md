---
date: 2026-06-02
domains:
- docs.python.org
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/thread-based-parallelism-python/thread-based-parallelism-python/
title: Thread-Based Parallelism in Python
---

{{% priors %}}

In Chromium, why wasn't explicit threading a thing? Data structures are usually
not thread-safe. There were guardrails for scenarios like checking `WeakPtr`s
on a different thread. However, I didn't come out of it with a mental model for
threading primitives.

{{% /priors %}}

## Introduction

Threads are smaller units of a process that allow executing tasks in parallel,
sharing memory space. Threads are particularly useful for I/O bound tasks,
where much of the time is spent waiting for external resources. {{% cite
threadingPy %}}

Compared to Java's threading model, Python's model has some differences. Locks
and condition variables are separate objects. Furthermore, there are no
priorities, no thread groups, and threads cannot be destroyed, stopped,
suspended, resumed, or interrupted. {{% cite threadingPy %}}

## The `Thread` Object

{{< readfile
  file="/content/computer-science/programming-challenges/language-concepts/resource-management/thread-based-parallelism-python/hello_threads.py"
  highlight="py" >}}

`t.start()` must be called at most once per `t`, lest it raises a
`RuntimeError`. {{% cite threadingPy %}}

{{% open-comment %}}

Why make it a hard error instead of no-op'ing on subequent `t.start()` calls?

{{% /open-comment %}}

`t.is_alive()` returns `True` just before the `t.run()` method starts until
after the `t.run()` method terminates. `threading.enumerate()` returns a list of
alive threads. {{% cite threadingPy %}}

If `run()` raises an exception, `threading.excepthook()` is called to handle it.
By default `threading.excepthook()` ignores `SystemExit`. {{% cite threadingPy
%}}

By default, `join(timeout=None)` blocks until the thread terminates.
`t.join(timeout=2.5)` blocks for ~2.5s. If `t` has not terminated by then,
`t.is_alive() == True`. A thread can be joined many times. {{% cite threadingPy
%}}

`t.daemon` must be set before `t.start()` is called, else a `RuntimeError`. The
initial value is inherited from the creating thread. The main thread is not a
daemon thread. The entire Python program exists when no alive non-daemon threads
are left. {{% cite threadingPy %}}

## Thread-Local Data

Thread-local data is data whose values are thread-specific, e.g.,

```py
mydata = local()
mydata.number = 42 # Stored inside mydata.__dict__

log = []

def f():
  items = sorted(mydata.__dict__.items())
  log.append(items)
  mydata.number = 11
  log.append(mydata.number)

thread = threading.Thread(target=f)
thread.start()
thread.join()
```

... where `log` will have two items, `[]`, and `11`. `mydata.number = 42`
executed on a different thread than the one `mydata.number = 11` executed on.
{{% cite threadingPy %}}

Subclasses of `local` can define `__slots__` that are shared across threads,
e.g.,

```py
class MyLocal(local):
  __slots__ = "number"

mydata = MyLocal()
mydata.number = 42

thread = threading.Thread(target=f)
thread.start()
thread.join()
```

... where `mydata.number == 11`. {{% cite threadingPy %}}

## `Lock` Objects

A `Lock` is a synchronization primitive that assumes one of two states: `locked`
and `unlocked`. It is not owned by a particular thread when locked. {{% cite
threadingPy %}}

If `acquire(blocking=True, timeout=-1)` returns `True`, then `l.locked() ==
True`. Using `blocking=True` blocks until the lock is unlocked, and then locks
it and returns `True`. If `blocking=False`, then `acquire` only locks if `l` was
already unlocked. `timeout=-1` specifies an unbounded wait; it's an error to do
`l.acquire(blocking=False, timeout=2.5)`. {{% cite threadingPy %}}

`release()` releases a lock, and can be called from any thread. `l.release()` on
an already unlocked `Lock` raises a `RuntimeError`. If any other threads are
blocked waiting on `l` to become unlocked, `l.release()` allows exactly one of
them to proceed. {{% cite threadingPy %}}

## `RLock` Objects

A reentrant lock is a synchronization primitive that may be acquired multiple
times by the same thread. It is owned by a particular thread when locked; in the
unlocked state, no thread owns it. {{% cite threadingPy %}}

`acquire()`/`release()` must be called in pairs. Failing to call `release()` as
many times as `acquire()` can lead to deadlock. {{% cite threadingPy %}}

`acquire(blocking=True, timeout=-1)`. If no thread owns `l`, acquire the lock
and return `True`. If the same thread owns `l`, acquire the lock again and
return `True` (`Lock` would have blocked until `l` can be acquired). If another
thread owns the lock:

* If `blocking=True`, block until we can acquire the lock, while respecting
  `timeout`.
* Otherwise, return `False` as we cannot acquire the lock.

{{% cite threadingPy %}}

{{% comment %}}

Doesn't reentrancy on `Lock` always cause deadlock? If `t` locked `l`, how can
`t` unlock `l` while its own execution is blocked waiting for "someone" to
unlock `t`? Ah, some other thread `t2` can call `l.release()`.

{{% /comment %}}

`release()` decrements the recursion level. If the new recursion level is zero,
`l` is unlocked (not owned by any thread) and exactly one of any pending threads
proceed. If the recursion level is still non-zero, `l` remains locked and owned
by the calling thread. `RuntimeError` results if `release()` is called on a lock
that is not acquired. {{% cite threadingPy %}}

## Condition Variables

A condition variable is always associated with some kind of lock, e.g.,

```py
cv = thread.Condition(lock=l) # cv has acquire(), release(), and locked() targeting l

with cv:
  while not an_item_is_available(): # Can be replaced w/ cv.wait_for(an_item_is_available)
    cv.wait()
  get_an_available_item()

with cv:
  make_an_item_available()
  cv.notify()
```

{{% cite threadingPy %}}

`wait(timeout=None)` releases the underlying lock (`RuntimeError` is the lock
had not been acquired) and blocks until another thread awakens it via a
`notify()`/`notify_call()` call. Once awakened, `wait()` re-acquires the lock
and returns. `wait_for(predicate, timeout=None)` follows the same rules as
`wait`. {{% cite threadingPy %}}

{{% comment %}}

Think in terms of invariants. The calling thread owns the lock before `wait()`
and after `wait()`, no matter what happens.

{{% /comment %}}

The `while` loop (and the `cv.wait_for` utility) is needed because `wait()` can
return after an arbitrary long time, and the condition that prompted `notify()`
may no longer hold true. This is inherent to multi-threaded programming. {{%
cite threadingPy %}}

`notify(n=1)` and `notify_all()` don't release the lock (and so awakened threads
don't return from their `wait()` call immediately). The thread that called
`notify()` needs to relinquish ownership of the lock. `RuntimeError` if the
calling thread hasn't acquired the lock when calling `notify*()`. {{% cite
threadingPy %}}

`notify(n=1)` wakes up at most \\(n\\) threads, and no-ops if no threads are
waiting. `notify_all()` wakes up all threads waiting on this condition. In a
typical producer-consumer situation, adding one item to the buffer only needs to
wake up one consumer thread. {{% cite threadingPy %}}

## Semaphore Objects

`Semaphore` manages an internal counter which is decremented by each
`acquire(blocking=True, timeout=None)` and incremented by each `release()`. When
the counter is \\(0\\) on an `acquire()` call, the semaphore blocks, waiting
until some other thread calls `release(n=1)`. {{% cite threadingPy %}}

`BoundedSemaphore(value=1)` checks that its counter never exceeds its initial
value, and raises `ValueError` if it does. `BoundedSemaphore`s guard resources
with limited capacity; releasing too many times is a sign of a bug. {{% cite
threadingPy %}}

## Event Objects

`threading.Event` affords a simple communication mechanism: one thread signals
an event and other threads wait for it. `Event` manages a flag that can be
modified via `set()` and `clear()` and queried via `is_set()`.
`wait(timeout=None)` blocks as long as `ev.is_set() == False` and the timeout,
if given, has not expired. {{% cite threadingPy %}}

## Timer Objects

`Timer` subclasses `Thread` and represents an action that should be run only
after a certain amount of time has passed, e.g.,

```py
def hello(name):
  print(f"hello, {name}")

t = Timer(30.0, hello, args=("Chege",))
t.start() # After 30s, "hello, world" will be printed. We can cancel() before 30s elapses.
```

{{% cite threadingPy %}}

## Barrier Objects

`Barrier` is a synchronization primitive for use by \\(n\\) threads that need to
wait for each other, e.g.,

```py
b = Barrier(2, timeout=None, action=None)

def server():
  start_server()
  b.wait()
  while True:
    connection = accept_connection()
    process_server_connection(connection)

def client():
  b.wait()
  while True:
    connection = make_connection()
    process_client_connection(connection)
```

{{% cite threadingPy %}}

`wait(timeout=None)` passes the barrier. When all \\(n\\) threads call `wait()`,
they are all released simultaneously. `wait` returns an integer \\(x \in [0,
N)\\), which can be used for housekeeping, e.g., printing something. If `action`
was passed in the constructor, one of the threads will have called it prior to
being released. {{% cite threadingPy %}}

A `Barrier` transitions into a broken state if `action` throws, or `abort()` is
called. Any active and future `wait()` calls fail with `BrokenBarrierError`.
Instead of aborting, it's simpler to specify a sensible timeout value. `reset()`
returns the barrier to the default, empty state and raises `BrokenBarrierError`
for waiting threads. {{% cite threadingPy %}}

## References

1. {{< citation
  id="threadingPy"
  title="threading — Thread-based parallelism — Python 3.14.5 documentation"
  url="https://docs.python.org/3/library/threading.html"
  accessed="2026-06-02" >}}

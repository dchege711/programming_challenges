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
many times as `acquire()` can lead to dreadlock. {{% cite threadingPy %}}

`acquire(blocking=True, timeout=-1)`. If no thread owns `l`, acquire the lock
and return `True`. If the same thread owns `l`, acquire the lock again and
return `True` (`Lock` would have blocked until `l` can be acquired). If another
thread owns the lock:

* If `blocking=True`, block until we can acquire the lock, while respecting
  `timeout`.
* Otherwise, return `False` as we cannot acquire the lock.

{{% cite threadingPy %}}

{{% comment %}}

Doesn't reentrancy on `Lock` always cause dreadlock? If `t` locked `l`, how can
`t` unlock `l` while its own execution is blocked waiting for "someone" to
unlock `t`? Ah, some other thread `t2` can call `l.release()`.

{{% /comment %}}

`release()` decrements the recursion level. If the new recursion level is zero,
`l` is unlocked (not owned by any thread) and exactly one of any pending threads
proceed. If the recursion level is still non-zero, `l` remains locked and owned
by the calling thread. `RuntimeError` results if `release()` is called on a lock
that is not acquired. {{% cite threadingPy %}}

## References

1. {{< citation
  id="threadingPy"
  title="threading — Thread-based parallelism — Python 3.14.5 documentation"
  url="https://docs.python.org/3/library/threading.html"
  accessed="2026-06-02" >}}

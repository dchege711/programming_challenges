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

## References

1. {{< citation
  id="threadingPy"
  title="threading — Thread-based parallelism — Python 3.14.5 documentation"
  url="https://docs.python.org/3/library/threading.html"
  accessed="2026-06-02" >}}

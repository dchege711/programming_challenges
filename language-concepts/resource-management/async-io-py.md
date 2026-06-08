---
date: 2026-06-07
domains:
- docs.python.org
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/async-io-py/
title: AsyncIO in Python
---

{{% comment %}}

Only covers the high level building blocks of `asyncio`. {{% cite PyAsyncIO %}}
has a second section on how `asyncio` and `await` internally work.

{{% /comment %}}

## The Event Loop

The **event loop** contains a collection of jobs to be run. Some jobs are added
by application code, and others indirectly by `asyncio`. The event loop takes a
job from its backlog and invokes it ("gives it control"). Once that job pauses
or completes, it returns control to the event loop which then selects another
job from the backlog and invokes it. When there are no jobs to be done, the
event loop rests (doesn't waster CPU cycles) and comes back when more jobs are
enqueued.

```py
import asyncio

# For demonstration. Usually, application code delegates event loop management
# to `asyncio` and calls `asyncio.run` instead.
event_loop = asyncio.new_event_loop()
event_loop.run_forever()
```

{{% cite PyAsyncIO %}}

## Coroutine Functions and Coroutines

`async def` creates an async function or **coroutine function**. Calling the
coroutine function creates a coroutine object; it doesn't execute the function.
A coroutine has to be explicitly started.

```repl
>>> async def f(): print("Hello, world")
>>> c = f()
>>> c
<coroutine object f at 0x10a585a80>
```

{{% cite PyAsyncIO %}}

## Tasks

**Tasks** are coroutines that are tied to an event loop. `asyncio.create_task(c:
Coroutine)` creates such a task, and adds a callback to run it in the event
loop's collection of jobs.

```py
import asyncio

async def main():
  ...

if __name__ == "__main__":
  asyncio.run(main())
  # Program will not print this until the coroutine main() finishes
  print("coroutine main() is done!")
```

{{% cite PyAsyncIO %}}

Only a callback to the task is added to the event loop; the task itself is not:

```py
async def hello():
  print("hello")

async def main():
  # Because there is no reference to this task object, it might be garbage
  # collected before the event loop invokes it.
  asyncio.create_task(hello())

asyncio.run(main())
```

{{% cite PyAsyncIO %}}

## `await` Keyword

### Awaiting a Task

```py
async def plant_a_tree():
  dig_the_hole_task = asyncio.create_task(dig_the_hole())
  await dig_the_hole_task
  ...
```

Suppose the event loop has passed control to the start of the `plant_a_tree`
coroutine:

* `await dig_the_hole_task` adds a callback (which will resume `plant_a_tree()`)
  to `dig_the_hole_task`'s list of callbacks, and then cedes control to the
  event loop.
* Sometime later, the event loop passes control to `dig_the_hole_task`. Once the
  task finishes, it adds its various callbacks to the event loop, e.g., a call
  to resume `plant_a_tree()`.

{{% cite PyAsyncIO %}}

### Awaiting a Coroutine

`await coroutine` does not cede control back to the event loop. `await
coroutine` is effectively the same as invoking a regular, synchronous Python
function. {{% cite PyAsyncIO %}}

Consider:

```py
async def coro_a():
    print("I am coro_a(). Hi!")

async def coro_b():
    print("I am coro_b(). I sure hope no one hogs the event loop...")
```

Given:

```py
async def await_coroutines_a():
    task_b = asyncio.create_task(coro_b())
    for _ in range(3):
        await coro_a()
    await task_b
```

... `asyncio.run(await_coroutines_a())` prints:

```txt
I am coro_a(). Hi!
I am coro_a(). Hi!
I am coro_a(). Hi!
I am coro_b(). I sure hope no one hogs the event loop...
```

Compare this to:

```py
async def await_tasks_a():
    task_b = asyncio.create_task(coro_b())
    tasks_a = [ asyncio.create_task(coro_a()) for _ in range(3) ]
    for task_a in tasks_a:
        await task_a
    await task_b
```

... where `asyncio.run(await_tasks_a())` prints:

```txt
I am coro_b(). I sure hope no one hogs the event loop...
I am coro_a(). Hi!
I am coro_a(). Hi!
I am coro_a(). Hi!
```

{{% cite PyAsyncIO %}}

### Rationale for `await`'s Split Behavior

`asyncio.run()` has a `debug=True` flag that logs coroutines that monopolize
execution for +100ms, among other things. {{% cite PyAsyncIO %}}

`await coroutine`'s behavior trades conceptual clarity for improved performance.
`await task` needs to pass control all the way up the call stack to the event
loop. In a large program with many `await` statements and a deep call stack, the
overhead can add up noticeably. {{% cite PyAsyncIO %}}

## References

1. {{< citation
  id="PyAsyncIO"
  title="A Conceptual Overview of asyncio — Python 3.14.5 documentation"
  url="https://docs.python.org/3/howto/a-conceptual-overview-of-asyncio.html"
  accessed="2026-06-07" >}}

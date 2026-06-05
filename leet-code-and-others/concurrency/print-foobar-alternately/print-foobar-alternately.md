---
cited-authors:
- Usewik, Alex
date: 2026-06-04
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/concurrency/print-foobar-alternately/print-foobar-alternately/
title: Print FooBar Alternately
---

## Problem Statement

You're given the `FooBar` class:

```py
class FooBar:
  def __init__(self, n):
    self.n = n

  def foo(self, printFoo: "Callable[[], None]") -> None:
    for _ in range(self.n):
      printFoo()

  def bar(self, printBar: "Callable[[], None]") -> None:
    for _ in range(self.n):
      printBar()
```

... and the same instance of `FooBar` will be passed to two different threads.
Thread `A` will call `foo()` while thread `B` will call `bar()`. Modify the
program to output `foobar` `n` times. {{% cite LCPrintFooBarAlternately %}}

## Solution

### Two `Event`s

We can use two `Event`s to communicate whose turn it is to print.

<details>
<summary>Implementation: Two <code>Event</code>s</summary>

```py
from threading import Event
from typing import Callable


class FooBar:
    def __init__(self, n):
        self.n = n
        self.foos_turn = Event()
        self.bars_turn = Event()
        self.foos_turn.set()

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.foos_turn.wait()
            printFoo()
            self.foos_turn.clear()
            self.bars_turn.set()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.bars_turn.wait()
            printBar()
            self.bars_turn.clear()
            self.foos_turn.set()
```

</details>

{{% comment %}}

But I feel like there's a concept that I'm missing. The two-event system is
primarily a system for communicating ownership, and that's what `Lock`s are for.
However, a single `Lock` proved awkward to reason about.

{{% /comment %}}

### `Condition` Variable

Using a `Condition` is more idiomatic, and would scale better as \\(N > 2\\).

<details>
<summary>Implementation: One <code>Condition</code></summary>

```py
from threading import Condition
from enum import Enum
from functools import partial
from typing import Callable

class Turn(Enum):
    FOO = 1
    BAR = 2

class FooBar:
    def __init__(self, n):
        self.n = n
        self.condition = Condition()
        self.current_turn = Turn.FOO

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.FOO))
                printFoo()
                self.current_turn = Turn.BAR
                self.condition.notify()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.BAR))
                printBar()
                self.current_turn = Turn.FOO
                self.condition.notify()

    def _is_turn(self, turn: Turn) -> bool:
        return self.current_turn == turn
```

</details>

{{% comment %}}

{{% cite LCPrintFooBarAlternatelyAlex %}} has 5 solutions for `Barrier`, `Lock`,
`Semaphore`, `Condition`, and `Event`. Solving this problem in all 5 different
ways should help me digest [Thread-Based Parallelism in Python]({{< ref
"/computer-science/programming-challenges/language-concepts/resource-management/thread-based-parallelism-python/thread-based-parallelism-python"
>}}) further.

{{% /comment %}}

### Buggy `Barrier`

How can this be solved using `Barrier`s? It seems awkward to do because
`Barrier`s don't convey turn-based semantics; they read like one-way trap doors.
Aha, I missed the fact that a `Barrier` can be reused any number of times for
the same number of threads.

<details>
<summary>Implementation: One <code>Barrier</code></summary>

```py
from threading import Barrier
from typing import Callable

class FooBar:
    def __init__(self, n):
        self.n = n
        self.barrier = Barrier(2)

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            printFoo()
            self.barrier.wait()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.barrier.wait()
            printBar()
```

</details>

However, a `Barrier` is sufficient because it has the invariant "both reached
this point" and not turn order "foo then bar before next foo". Although {{% cite
LCPrintFooBarAlternately %}} accepts the above solution, `test_solution.py`
exposes a bug when `foo` is called first.

<details>
<summary><code>test_solution.py</code></summary>

{{< readfile
  file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/print-foobar-alternately/test_solution.py"
  highlight="py" >}}

</details>

### Two `Semaphore`s

Using two `BoundedSemaphore(value=1)` to further check that each gate's internal
counter is in the range \\([0, 1]\\).

<details>
<summary>Implementation: Two <code>BoundedSemaphore</code>s</summary>

```py
from threading import BoundedSemaphore
from typing import Callable

class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_gate = BoundedSemaphore(1)
        self.bar_gate = BoundedSemaphore(1)
        self.bar_gate.acquire()

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.foo_gate.acquire()
            printFoo()
            self.bar_gate.release()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.bar_gate.acquire()
            printBar()
            self.foo_gate.release()
```

</details>

### Two `Lock`s

`Lock` is appropriate here. `RLock` comes with a notion of recursion, and that
would complicate things unnecessarily.

<details>
<summary>Implementation: Two <code>Lock</code>s</summary>

```py
from threading import Lock
from typing import Callable

class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_lock = Lock()
        self.bar_lock = Lock()
        self.bar_lock.acquire()

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.foo_lock.acquire()
            printFoo()
            self.bar_lock.release()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.bar_lock.acquire()
            printBar()
            self.foo_lock.release()
```

</details>

### Remarks

Of the solutions, I like `Condition` the most because it supports \\(N > 2\\)
threads in a sane manner and the code itself reads closer to the scenario.

## References

1. {{< citation
  id="LCPrintFooBarAlternately"
  title="Print FooBar Alternately"
  url="https://leetcode.com/problems/print-foobar-alternately/description/"
  accessed="2026-06-04" >}}

1. {{< citation
  id="LCPrintFooBarAlternatelyAlex"
  author="Alex Usewik"
  title="Print FooBar Alternately"
  url="https://leetcode.com/problems/print-foobar-alternately/solutions/336629/5-python-threading-solutions-barrier-eve-jmla/"
  accessed="2026-06-04" >}}

---
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

{{< readfile
  file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/print-foobar-alternately/print_foobar_alternately.py"
  highlight="py" >}}

</details>

But I feel like there's a concept that I'm missing. The two-event system is
primarily a system for communicating ownership, and that's what `Lock`s are
for.

However, a single `Lock` expresses mutual exclusion, but doesn't encode whose
turn is next. Using a `Condition` is more idiomatic, and would scale better as
\\(N > 2\\).

<details>
<summary>Implementation: One <code>Condition</code></summary>

{{< readfile
  file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/print-foobar-alternately/print_foobar_alternately.py"
  highlight="py" >}}

</details>

## References

1. {{< citation
  id="LCPrintFooBarAlternately"
  title="Print FooBar Alternately"
  url="https://leetcode.com/problems/print-foobar-alternately/description/"
  accessed="2026-06-04" >}}

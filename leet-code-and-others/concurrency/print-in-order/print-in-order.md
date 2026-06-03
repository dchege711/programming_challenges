---
date: 2026-06-01
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/concurrency/print-in-order/print-in-order/
title: Print in Order
---

Suppose we have a class:

```cpp
public class Foo {
  public void first() { print("first"); }
  public void second() { print("second"); }
  public void third() { print("third"); }
}
```

... and the same instance of `Foo` is passed to three different threads. Thread
A will call `first()`, thread B will call `second()` and thread C will call
`third()`. Design `Foo` such that `second()` is executed after `first()`, and
`third()` is executed after `second()`. {{% cite LCPrintInOrder %}}

See [Thread-Based Parallelism - Python]({{< ref
"/computer-science/programming-challenges/language-concepts/resource-management/thread-based-parallelism-python/thread-based-parallelism-python"
>}}) for a quick primer on Python's synchronization primitives.

{{< readfile
  file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/print-in-order/PrintInOrder.py"
  highlight="py" >}}

1. {{< citation
  id="LCPrintInOrder"
  title="Print in Order - LeetCode"
  url="https://leetcode.com/problems/print-in-order/"
  accessed="2026-06-01" >}}

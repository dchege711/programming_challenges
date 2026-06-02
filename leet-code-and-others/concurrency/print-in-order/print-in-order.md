---
title: "Print in Order"
date: 2026-06-01
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

1. {{< citation
  id="LCPrintInOrder"
  title="Print in Order - LeetCode"
  url="https://leetcode.com/problems/print-in-order/"
  accessed="2026-06-01" >}}

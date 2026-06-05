---
date: 2026-06-05
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/concurrency/print-zero-even-odd/print-zero-even-odd/
title: Print Zero Even Odd
---

## Problem

`printNumber(7)` prints `7` to the console. You are given an instance of the
`ZeroEvenOdd` class with 3 functions: `zero` outputs `0`s, `even` outputs even
numbers, and `odd` outputs odd numbers. The same `ZeroEvenOdd` instance is
passed to three different threads, where thread A calls `zero()`, thread B calls
`even()`, and thread C calls `odd()`. Modify `ZeroEvenOdd` such that given
`n=6`, it outputs `010203040506`. {{% cite LCPrintZeroEvenOdd %}}

## Solution

Use a `Condition` to pass the baton between `zero`, `even`, and `odd`. I had
trouble setting up the correct loop-exit conditions. Debugging deadlocks takes
some getting used to!

{{< readfile
  file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/print-zero-even-odd/solution.py"
  highlight="py" >}}

## References

1. {{< citation
  id="LCPrintZeroEvenOdd"
  title="Print Zero Even Odd"
  url="https://leetcode.com/problems/print-zero-even-odd/description/"
  accessed="2026-06-05" >}}

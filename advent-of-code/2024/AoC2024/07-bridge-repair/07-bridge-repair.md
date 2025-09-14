---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
- docs.python.org
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/07-bridge-repair/
title: 'AoC 2024 Day 07: Bridge Repair'
---

## Parsing

Each line represents a single equation, e.g., `292: 11 6 16 20`. {{% cite
AoC2024Day07 %}}

[Part One](#part-one) needs to make a decision based on each line independently.
Parsing each line into a data structure and yielding that should suffice.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/BridgeRepair.Parse.cs"
  highlight="cs"
  id="BridgeRepair.Parse.cs" >}}

## Part One

Using only **add** and **multiply**, evaluated left-to-right (not according to
math precedence rules), determine which equations could possibly be true. For
example, \\(292 = ((11 + 6) \times 16) + 20\\). Of the equations that could
possibly be true, what is the sum of their results? {{% cite AoC2024Day07 %}}

Given a `CalibrationEquation` with \\(N + 1\\) operands, there are \\(2^N\\)
ways of selecting the \\(N\\) operators. Some algorithm optimizations to test
less than \\(2^N\\) permutations per `CalibrationEquation`:

* Given that we can only add or multiply, the result can never decrease. As soon
  as the result exceeds the expected result, we can discard the search.
* As soon as we get a valid combination, we can stop the search because we only
  need to find one combination that works.

Python's `itertools.product` is a built-in way of generating the \\(2^N\\)
permutations. {{% cite itertools %}} C# doesn't have an equivalent. Implementing
`itertools.product` seems non-trivial to do without lots of memory allocations.
Does a recursive solution get the job done? Yup!

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/BridgeRepair.PartOne.cs"
  highlight="cs"
  id="BridgeRepair.PartOne.cs" >}}

## Part Two

Consider the concatenation operator that combines its left and right operands
into a single number, e.g., \\(12\ ||\ 345 = 12345\\). For instance, \\(7290: 6\
8\ 6\ 15\\) can be made true using \\(((6 \times 8)\ ||\ 6) \times 15\\). Of the
equations that could possibly be true by any combination of the three operators,
what is the sum of their results? {{% cite AoC2024Day07 %}}

The core algorithm doesn't change, but we now have \\(3^N\\) permutations to
consider for each `CalibrationEquation`. Why did the runtime go from 2s for part
1 to 62s for part 2? It shouldn't be that drastic...

Maybe it's in concatenation's implementation? `long.Parse($"{a}{b}")` might be
too inefficient to call in a hot path. If \\(b\\) has \\(m\\) digits, then \\(a
\times 10^m + b\\) should be much faster because math operations are faster than
string parsing operations. Using this implementation:

```cs
var (factor, reducedB) = (10L, b);
while ((reducedB /= 10) > 0)
    factor *= 10;
return (a * factor) + b;
```

... still runs in ~60s, implying that `long.Parse($"{a}{b}")` is not the source
of the slowdown. Either the arithmetic approach is just as slow, or majority of
the time is spent searching the \\(3^N\\) permutations.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/BridgeRepair.PartTwo.cs"
  highlight="cs"
  id="BridgeRepair.PartTwo.cs" >}}

## References

1. {{< citation
  id="AoC2024Day07"
  title="Day 07 - Advent of Code 2024: Bridge Repair"
  url="https://adventofcode.com/2024/day/7"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

1. {{< citation
  id="itertools.product"
  title="itertools — Functions creating iterators for efficient looping — Python 3.13.7 documentation"
  url="https://docs.python.org/3/library/itertools.html"
  accessed="2025-09-13" >}}

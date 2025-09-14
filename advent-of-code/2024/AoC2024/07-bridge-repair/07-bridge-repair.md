---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
- docs.python.org
- en.wikipedia.org
- learn.microsoft.com
- stackoverflow.com
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
A recursive implementation of `itertools.product` gets the job done:

```cs
private static IEnumerable<ImmutableList<Operator>> PermutationWithReplacement(
    ImmutableHashSet<Operator> seedOperators, ImmutableList<Operator> operators, int desiredLength)
{
    if (operators.Count == desiredLength)
        yield return operators;
    
    if (operators.Count > desiredLength)
        yield break;
    
    foreach (var @operator in seedOperators)
        foreach (var permutation in PermutationWithReplacement(seedOperators, operators.Add(@operator), desiredLength))
            yield return permutation;
}
```

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

What about finding \\(m\\) using \\(\left \lfloor{log_{10}(n)} \right \rfloor +
1\\) instead of the iterative approach? {{% cite StackOverflow6655754 %}} No
dice; also runs in ~60s. `Math.Pow` returns a `double`, so for the sake of
avoiding precision loss and casting, reverting to the iterative approach.

Evaluating each `CalibrationEquation` is an embarrassingly parallel problem in
that no evaluation needs to communicate with any other evaluation {{% cite
WikiEmbarrassinglyParallel %}}. Adding `AsParallel` gets us from 60s to 30s. {{%
cite PLINQ %}}

Granted, `PermutationWithReplacement` is inefficient compared to Python's
`itertools.product` which does not build up intermediate results in memory. {{%
cite itertools %}} That said, does `PermutationWithReplacement(...).Any(...)`
stop as early as possible? Logs for \\(7290: 6\ 8\ 6\ 15\\) show that while we
do stop early, we evaluate the same expression multiple times. Aha, given
\\(N\\) operands, we're asking `PermutationWithReplacement` to generate
\\(3^N\\) permutations of operators instead of generating \\(3^{N-1}\\)
permutations. Fixing that brings us from 30s to 10s. A 3X improvement, but 10s
still seems slow.

{{% comment %}}

`Enumerable.Zip` stops at the shorter of the two subsequences; there's no
exception thrown when the two sequences have differing lengths. {{% cite
Enumerable.Zip %}}

{{% /comment %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/BridgeRepair.PartTwo.cs"
  highlight="cs"
  id="BridgeRepair.PartTwo.cs" >}}

What if instead using `PermutationWithReplacement`, we have a recursive
`IsValid` that exits early? Huh, solving part 2 goes from 10s to ~1s; didn't
expect that much of a speedup!

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/BridgeRepair.Alternate.cs"
  highlight="cs"
  id="BridgeRepair.Alternate.cs" >}}

The recursive `IsValid` has an early return advantage. In a line like
\\(123456:\ 123\ 456\ 9\\), `IsValid` returns early after evaluating \\(123\ ||\
456 \\) and noticing that we still have 1 more non-decreasing operator to apply.
In the `PermutationWithReplacement` approach, we have still generated operators
for \\(123\ ||\ 456 + 9\\), \\(123\ ||\ 456\ || 9\\), and \\(123\ ||\ 456 \times
9\\) before abandoning this part of the search space. Again, hindsight is 20/20.

## References

1. {{< citation
  id="AoC2024Day07"
  title="Day 07 - Advent of Code 2024: Bridge Repair"
  url="https://adventofcode.com/2024/day/7"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

1. {{< citation
  id="itertools"
  title="itertools — Functions creating iterators for efficient looping — Python 3.13.7 documentation"
  url="https://docs.python.org/3/library/itertools.html"
  accessed="2025-09-13" >}}

1. {{< citation
  id="StackOverflow6655754"
  title="algorithm - Finding the number of digits of an integer - Stack Overflow"
  url="https://stackoverflow.com/questions/6655754/finding-the-number-of-digits-of-an-integer"
  accessed="2025-09-13" >}}

1. {{< citation
  id="WikiEmbarrassinglyParallel"
  title="Embarrassingly parallel - Wikipedia"
  url="https://en.wikipedia.org/wiki/Embarrassingly_parallel"
  accessed="2025-09-13" >}}

1. {{< citation
  id="PLINQ"
  title="Introduction to PLINQ - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/introduction-to-plinq"
  accessed="2025-09-13" >}}

1. {{< citation
  id="Enumerable.Zip"
  title="Enumerable.Zip Method (System.Linq) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.zip?view=net-9.0"
  accessed="2025-09-14" >}}

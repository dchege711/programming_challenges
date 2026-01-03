---
cited-authors:
- Wastl, Eric
date: 2025-12-07
domains:
- adventofcode.com
title: 'AoC 2024 Day 13: Claw Contraption'
---

## Parsing

The input is a list of machine configurations, where buttons \\(A\\) and \\(B\\)
move the claw some distance \\(X\\) and \\(Y\\), and the location of the prize
is specified. {{% cite AoC2024Day13 %}}

```txt
Button A: X+43, Y+68
Button B: X+10, Y+36
Prize: X=4800, Y=6250

Button A: X+63, Y+41
Button B: X+89, Y+18
Prize: X=17648, Y=19276
```

{{< readFile
  file="content/computer-science/programming_challenges/advent-of-code/2024/AoC2024/13-claw-contraption/ClawContraption.DataTypes.cs"
  highlight="cs"
  id="ClawContraption.DataTypes.cs" >}}

## Part One

It costs \\(3\\) tokens to push the \\(A\\) button and \\(1\\) token to push the
\\(B\\) button. What is the fewest tokens you would have to spend to win all
possible prizes? {{% cite AoC2024Day13 %}}

A brute force approach would have us start from \\((0, 0)\\), and choose whether
to push \\(A\\) or \\(B\\) at each step. All of the buttons in the input have
\\(X > 0\\) and \\(Y > 0\\), and so we don't backtrack. However, it's possible
that some later path in the exploration yields a less costly path to \\((x,
y)\\), and we'd need to recompute the sub-paths starting from \\((x, y)\\). This
can get computationally expensive real fast.

{{% cite AoC2024Day13 %}} reads like a shortest paths graph problem. Some
properties of this graph:

* The nodes of the graph contain the origin \\((0, 0)\\), and any other points
  \\((x_i, y_i)\\) that are reachable through some combination of \\(A\\) and
  \\(B\\) moves.
* The prize coordinate, \\((x_p, y_p)\\), is not a part of the graph if there
  is no path to it.
* There is no need for a node \\((x, y)\\) where \\(x > x_p\\) or \\(y > y_p\\)
  because we can't reach \\((x_p, y_p)\\) from such a node.
* The edges have weights of either \\(3\\) or \\(1\\) depending on whether
  \\(A\\) or \\(B\\) was used to connect the two nodes.

The [core implementation](#core-implementation) is common to both sub-problems.

{{< readFile
  file="content/computer-science/programming_challenges/advent-of-code/2024/AoC2024/13-claw-contraption/ClawContraption.PartOne.cs"
  highlight="cs"
  id="ClawContraption.PartOne.cs" >}}

## Part Two

Add \\(10,000,000,000,000\\) to the \\(X\\) and \\(Y\\) position of every prize.
What is the fewest tokens you would have to spend to win all possible prizes?
{{% cite AoC2024Day13 %}}

The [core implementation](#core-implementation) is common to both sub-problems.
Part Two makes inefficient implementations infeasible because of the sheer
number of possible \\(A\\) and \\(B\\) moves. In [part 1](#part-one), the prize
was always within \\(20,000\\) of either \\(X\\) or \\(Y\\). Moving the prize
at least \\(500,000,000\\) times away does not bode well.

{{< readFile
  file="content/computer-science/programming_challenges/advent-of-code/2024/AoC2024/13-claw-contraption/ClawContraption.PartTwo.cs"
  highlight="cs"
  id="ClawContraption.PartTwo.cs" >}}

## Core Implementation

{{% comment %}}

Relevant notes:

* [Shortest Paths in a Graph]({{< ref
  "/computer-science/algorithms-and-data-structures/graphs/shortest-paths/" >}})
* [C# Performance Tools]({{< ref
  "/computer-science/programming_challenges/advent-of-code/2024/AoC2024.Benchmarks/readme.md">}})

{{% /comment %}}

The implementation is too slow to solve [part 2](#part-two) in a reasonable
time. Solving the 320 configurations for [part 1](#part-one) takes \\(\approx
3.8s\\), and the memory usage is concerning. Naively assuming the
\\(500,000,000\\) factor in [part 2](#part-two) blows up the time and space
usage beyond practicality.

From `dotnet run -c Release -- -f '*Day13ClawContraption*'`:

| Method           | Mean    | Error    | StdDev   | Gen0        | Gen1        | Gen2       | Allocated |
|----------------- |--------:|---------:|---------:|------------:|------------:|-----------:|----------:|
| PartOneBenchmark | 3.834 s | 0.0647 s | 0.0540 s |     589,000 |     193,000 |     40,000 |   3.49 GB |

Using `struct` instead of `class` for `Vector`, `Button`, and `MachineConfig`
shaves 23% off the running time and biases towards more `Gen0` collections.

| Method           | Mean    | Error    | StdDev   | Gen0        | Gen1       | Gen2       | Allocated |
|----------------- |--------:|---------:|---------:|------------:|-----------:|-----------:|----------:|
| PartOneBenchmark | 2.949 s | 0.0583 s | 0.0648 s |     650,000 | 33,000     |     22,000 |      4 GB |

Making `Vector.GetEdges` write to a `Span<DirectedEdge>` owned by the caller
relieves some memory pressure, but the runtime only decreased by 3%. Reverting
{{% cite "programming_challenges@33318f0" %}} as the speed boost isn't worth the
indirection.

| Method           | Mean    | Error    | StdDev   | Gen0        | Gen1       | Gen2       | Allocated |
|----------------- |--------:|---------:|---------:|------------:|-----------:|-----------:|----------:|
| PartOneBenchmark | 2.853 s | 0.0290 s | 0.0257 s |     490,000 |     31,000 |     22,000 |   3.09 GB |

PLINQ seems to be making the analysis for `dotnet-trace` harder; let's remove
the `AsParallel` calls for now. We're almost back where we started memory-wise,
and the running time has ballooned. PLINQ is a winner  for {{% cite
AoC2024Day13 %}}, an embarrasingly parallel problem; restore `AsParallel` once
done evaluating the profile traces.

| Method           | Mean    | Error   | StdDev  | Gen0        | Gen1       | Gen2       | Allocated |
|----------------- |--------:|--------:|--------:|------------:|-----------:|-----------:|----------:|
| PartOneBenchmark | 13.47 s | 0.016 s | 0.015 s |     665,000 |     92,000 |     49,000 |      4 GB |

{{< readFile
  file="content/computer-science/programming_challenges/advent-of-code/2024/AoC2024/13-claw-contraption/ClawContraption.Common.cs"
  highlight="cs"
  id="ClawContraption.Common.cs" >}}

## References

1. {{< citation
  id="AoC2024Day13"
  author="Eric Wastl"
  title="Day 13 - Advent of Code 2024: Claw Contraption"
  url="https://adventofcode.com/2024/day/13"
  accessed="2025-12-07" >}}

1. {{< citation
  id="programming_challenges@33318f0"
  title="[AoC 2024] [Claw Contraption] Have Vector.GetEdges modify unowned Span · dchege711/programming_challenges@33318f0 · GitHub"
  url="https://github.com/dchege711/programming_challenges/commit/33318f0f1a9fcbffee305e9ed6ce05ac2f0a0d1b"
  accessed="2026-01-02" >}}
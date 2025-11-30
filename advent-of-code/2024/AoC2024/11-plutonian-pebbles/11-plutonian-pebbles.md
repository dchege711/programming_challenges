---
title: 'AoC 2024 Day 11: Plutonian Pebbles'
date: 2025-11-29
---

## Parsing

The stones are in a line, with each stone having a number engraved on it. {{%
cite AoC2024Day10 %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/11-plutonian-pebbles/PlutonianPebbles.Parse.cs"
  highlight="cs"
  id="PlutonianPebbles.Parse.cs" >}}

```cs
public static IEnumerable<ulong> ReadStones(string filePath)
{
    using StreamReader inputReader = new(filePath);
    return inputReader.ReadLine()?.Split().Select(ulong.Parse) ?? [];
}
```

The above snippet contains a subtle bug. The `using` statement disposes the
`StreamReader` before the `IEnumerable<ulong>` is consumed. However, because
`ReadLine` eagerly reads the content, there is no bug in this case.

## Setup

Every time you blink, the stones each simultaneously change based on the first
applicable rule in this list:

1. A stone engraved with `0` is replaced by a stone engraved with `1`.
2. A stone engraved with a number with an even number of digits is replaced by
   two stones. The left half of the digits are engraved in the new left stone,
   and the right half of the digits are engraved in the new right stone.
3. A stone is replaced by a new stone with an engraving equal to that of the old
   stone multiplied by 2024.

No matter how the stones change, their order is preserved. {{% cite AoC2024Day10
%}}

## Part One & Two

How many stones will you have after blinking 25 times? How about after 75 times?
{{% cite AoC2024Day10 %}}

The problem is embarrassingly parallel in that the expansion of each stone can
be computed independently. However, the same sub-problems (given a stone and a
number of blinks to perform) might be solved repeatedly. Utilizing a cache in
this case seems more important than trying to explore parallelism.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/11-plutonian-pebbles/PlutonianPebbles.Common.cs"
  highlight="cs"
  id="PlutonianPebbles.Common.cs" >}}

Alternatively, we can count the number of stones in each blink. This offers more
memory efficiency because \\(N\\) stones with the same value are represented
compactly -- the full array is never expanded. The compact representation also
avoids repeated computation. Caching in this case doesn't help because we never
encounter the same sub-problem twice given that we're considering blinks in
lockstep.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/11-plutonian-pebbles/PlutonianPebbles.DP.cs"
  highlight="cs"
  id="PlutonianPebbles.DP.cs" >}}

In {{% cite "PlutonianPebbles.Common.cs" %}}, I tried using PLINQ in
`CountStonesAfterBlinks`, only to find the program ran slower. {{% cite
PLINQPitfalls %}} notes that over-parallelization is a common pitfall,
especially in nested queries. PLINQ is probably helpful when:

1. The inner data source is known to be very long.
2. Each inner computation is expensive.
3. The target system has enough processors to handle the number of threads.

In {{% cite AoC2024Day10 %}} (2) and (3) are false. The overhead of
parallelization gets in the way.

## References

1. {{< citation
  id="AoC2024Day10"
  title="Day 11 - Advent of Code 2024: Plutonian Pebbles"
  author="Eric Wastl"
  url="https://adventofcode.com/2024/day/11"
  accessed="2025-11-29" >}}

1. {{< citation
  id="PLINQPitfalls"
  title="Potential pitfalls with PLINQ - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/potential-pitfalls-with-plinq"
  accessed="2025-11-29" >}}
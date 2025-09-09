---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
- learn.microsoft.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/05-print-queue/05-print-queue/
title: 'AoC 2024 Day 05: Print Queue'
---

## Parsing

The notation `X|Y` means that if both page number `X` and page number `Y` are to
be produced as part of an update, page number `X` **must** be printed at some
point before page number `Y`.

The input contains page ordering rules (pairs of `X|Y`) and print queues (e.g.,
`75, 47, 61, 53, 29`).

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/05-print-queue/PrintQueue.Parse.cs"
  highlight="cs"
  id="PrintQueue.Parse.cs" >}}

## Part One

Determine which updates are already in the correct order. What do you get if
you add up the middle page number from those correctly-ordered updates?

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/05-print-queue/PrintQueue.PartOne.cs"
  highlight="cs"
  id="PrintQueue.PartOne.cs" >}}

While {{% cite Enumerable.Index %}} returns an `IEnumerable` that incorporates
each element index into a tuple, {{% cite Enumerable.Select %}} passes the index
of the source element as the second parameter of the selector function. No need
to call `Index` when we don't need an explicit tuple.

## Part Two

For each of the incorrectly-ordered updates, use the page ordering rules to put
the page numbers in the right order, e.g., `61, 13 ,29` becomes `61, 29, 13`.
What do you get if you add up the middle page numbers after correctly ordering
just those updates?

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/05-print-queue/PrintQueue.PartTwo.cs"
  highlight="cs"
  id="PrintQueue.PartTwo.cs" >}}

## References

1. {{< citation
  id="AoC2024Day05"
  title="Day 5 - Advent of Code 2024: Print Queue"
  url="https://adventofcode.com/2024/day/5"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

1. {{< citation
  id="Enumerable.Index"
  title="Enumerable.Index<TSource>(IEnumerable<TSource>) Method (System.Linq) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.index?view=net-9.0"
  accessed="2025-09-09" >}}

1. {{< citation
  id="Enumerable.Select"
  title="Enumerable.Select Method (System.Linq) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.select?view=net-9.0"
  accessed="2025-09-09" >}}

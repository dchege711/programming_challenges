---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/05-print-queue/05-print-queue/
title: 'AoC 2024 Day 05: Print Queue'
---

## Data

The notation `X|Y` means that if both page number `X` and page number `Y` are to
be produced as part of an update, page number `X` **must** be printed at some
point before page number `Y`.

## Parsing

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

## References

1. {{< citation
  id="AoC2024Day05"
  title="Day 5 - Advent of Code 2024: Print Queue"
  url="https://adventofcode.com/2024/day/5"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

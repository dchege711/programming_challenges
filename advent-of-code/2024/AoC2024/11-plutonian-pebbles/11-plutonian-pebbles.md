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

## References

1. {{< citation
  id="AoC2024Day10"
  title="Day 11 - Advent of Code 2024: Plutonian Pebbles"
  author="Eric Wastl"
  url="https://adventofcode.com/2024/day/11"
  accessed="2025-11-29" >}}
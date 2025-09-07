---
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/04-ceres-search/04-ceres-search/
title: 'AoC 2024 Day 04: Ceres Search'
---

## Problem

As the search for the Chief continues, a small Elf who lives on the station tugs
on your shirt; she'd like to know if you could help her with her **word search**
(your puzzle input). She only has to find one word: `XMAS`.

This word search allows words to be horizontal, vertical, diagonal, written
backwards, or even overlapping other words. It's a little unusual, though, as
you don't merely find one instance of `XMAS` -- you need to find **all of
them.** Here are a few ways `XMAS` might appear, where irrelevant characters
have been replaced with `.`:

```txt
..X...
.SAMX.
.A..A.
XMAS.S
.X....
```

The actual word search will be full of letters instead. For example:

```txt
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
```

## Parsing

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/04-ceres-search/CeresSearch.Parse.cs"
  highlight="cs"
  id="CeresSearch.Parse.cs" >}}

## Part One

In this word search, `XMAS` occurs a total of `18` times; here's the same word
search again, but where letters not involved in any `XMAS` have been replaced
with `.`:

| | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | . | . | . | . | X | X | M | A | S | . |
| 1 | . | S | A | M | X | M | S | . | . | . |
| 2 | . | . | . | S | . | . | A | . | . | . |
| 3 | . | . | A | . | A | . | M | S | . | X |
| 4 | X | M | A | S | A | M | X | . | M | M |
| 5 | X | . | . | . | . | . | X | A | . | A |
| 6 | S | . | S | . | S | . | S | . | S | S |
| 7 | . | A | . | A | . | A | . | A | . | A |
| 8 | . | . | M | . | M | . | M | . | M | M |
| 9 | . | X | . | X | . | X | M | A | S | X |

Take a look at the little Elf's word search. **How many times does XMAS
appear**?

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/04-ceres-search/CeresSearch.PartOne.cs"
  highlight="cs"
  id="CeresSearch.PartOne.cs" >}}

Tripped up the first time. This isn't a valid `XMAS` occurrence because it's not
horizontal, vertical, diagonal, written backwards, or even overlapping other
`XMAS`. In some sense, `XMAS` needs to be in one direction.

| | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | M | M | M | S | <span style="color: red;">X</span> | X | M | A | S | M |
| 1 | M | S | A | M | X | <span style="color: red;">M</span> | S | M | S | A |
| 2 | A | M | X | S | X | M | <span style="color: red;">A</span> | A | M | M |
| 3 | M | S | A | M | A | <span style="color: red;">S</span> | M | S | M | X |
| 4 | X | M | A | S | A | M | X | A | M | M |
| 5 | X | X | A | M | M | X | X | A | M | A |
| 6 | S | M | S | M | S | A | S | X | S | S |
| 7 | S | A | X | A | M | A | S | A | A | A |
| 8 | M | A | M | M | M | X | M | M | M | M |
| 9 | M | X | M | X | A | X | M | A | S | X |

## References

1. {{< citation
  id="AoC2024Day04"
  title="Day 4 - Advent of Code 2024: Ceres Search"
  url="https://adventofcode.com/2024/day/4"
  accessed="2025-08-23" >}}

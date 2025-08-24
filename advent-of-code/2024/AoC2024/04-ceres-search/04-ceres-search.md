---
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/04-ceres-search/04-ceres-search/
title: 'AoC 2024 Day 04: Ceres Search'
---

## Problem Statement

### Part One

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

In this word search, `XMAS` occurs a total of `18` times; here's the same word
search again, but where letters not involved in any `XMAS` have been replaced
with `.`:

```txt
....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
```

Take a look at the little Elf's word search. **How many times does XMAS
appear**?

1. {{< citation
  id="AoC2024Day04"
  title="Day 4 - Advent of Code 2024: Ceres Search"
  url="https://adventofcode.com/2024/day/4"
  accessed="2025-08-23" >}}

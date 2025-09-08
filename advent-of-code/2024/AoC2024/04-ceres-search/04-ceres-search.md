---
cited-authors:
- Mirkovic, Dusko
date: 2025-08-23
domains:
- adventofcode.com
- code-maze.com
- learn.microsoft.com
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

For \\(M \times N\\) grids, C# has multi-dimensional arrays. These differ from
jagged arrays, whose elements are arrays with possibly different sizes. {{% cite
Array %}} In memory, multi-dimensional arrays are laid out as 1D arrays. When
processing array data, cache locality impacts performance. If the computation
needs to access multiple rows (e.g., image convolution, this problem), then
multi-dimensional arrays offer an advantage. If processing row-by-row, then
jagged arrays will also have cache locality. However, if the array is larger
than 85Kb, then it ends in the large object heap, which is slower. {{% cite
Mirkovic2024 %}}

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

The declarative form packs quite the punch:

```cs
return Enumerable.Range(0, grid.GetLength(0))
  .SelectMany(r => Enumerable.Range(0, grid.GetLength(1))
    .Select(c => NumOccurrences(r, c)))
  .Sum()
```

`r` is a variable that is in scope when `c => ...` is defined. When `c => ...`
uses `r`, it captures it. `r` is stored for use in `c => ...` even if `r` would
have otherwise gone out of scope. {{% cite LambdaExpressions %}}

## Part Two

This isn't actually an `XMAS` puzzle; it's an `X-MAS` puzzle in which you're
supposed to find two `MAS` in the shape of an `X`. One way to achieve that is
like this:

<table>
  <tr><td>M</td><td>.</td><td>S</td></tr>
  <tr><td>.</td><td>A</td><td>.</td></tr>
  <tr><td>M</td><td>.</td><td>S</td></tr>
</table>

Within the `X`, each `MAS` can be written forwards or backwards. Using the
example from above with all of the `X-MAS`es kept:

| | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | . | M | . | S | . | . | . | . | . | . |
| 1 | . | . | A | . | . | M | S | M | S | . |
| 2 | . | M | . | S | . | M | A | A | . | . |
| 3 | . | . | A | . | A | S | M | S | M | . |
| 4 | . | M | . | S | . | M | . | . | . | . |
| 5 | . | . | . | . | . | . | . | . | . | . |
| 6 | S | . | S | . | S | . | S | . | S | . |
| 7 | . | A | . | A | . | A | . | A | . | . |
| 8 | M | . | M | . | M | . | M | . | M | . |
| 9 | . | . | . | . | . | . | . | . | . | . |

In this example, an `X-MAS` appears `9` times. **How many times does an `X-MAS`
appear**?

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/04-ceres-search/CeresSearch.PartTwo.cs"
  highlight="cs"
  id="CeresSearch.PartTwo.cs" >}}

## References

1. {{< citation
  id="AoC2024Day04"
  title="Day 4 - Advent of Code 2024: Ceres Search"
  url="https://adventofcode.com/2024/day/4"
  accessed="2025-08-23" >}}

1. {{< citation
  id="Array"
  title="The array reference type - C# reference | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/arrays"
  accessed="2025-09-07" >}}

1. {{< citation
  id="Mirkovic2024"
  date="2024-03-22"
  author="Dusko Mirkovic"
  title="Multidimensional Array vs Jagged Array in C#"
  url="https://code-maze.com/charp-multidimensional-jagged-array/"
  accessed="2025-09-07" >}}

1. {{< citation
  id="LambdaExpressions"
  title="Lambda expressions - Lambda expressions and anonymous functions - C# reference | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/lambda-expressions"
  accessed="2025-09-07" >}}

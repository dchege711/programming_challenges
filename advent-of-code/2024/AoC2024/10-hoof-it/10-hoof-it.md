---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
- en.wikipedia.org
- leetcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/10-hoof-it/10-hoof-it/
title: 'AoC 2024 Day 10: Hoof It'
---

## Parsing

The topographic map indicates the **height** at each position using a scale from
`0` (lowest) to `9` (highest), e.g.,

```txt
0123
1234
8765
9876
```

{{% cite AoC2024Day10 %}}

A hiking trail is any path that starts at `0`, ends at height `9`, and increases
by a height of exactly `1` at each step. Hiking trails never include diagonal
steps -- only up, down, left, or right. A trailhead is any position that starts
one or more hiking trails. {{% cite AoC2024Day10 %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/10-hoof-it/HoofIt.Parse.cs"
  highlight="cs"
  id="HoofIt.Parse.cs" >}}

## Part One

A trailhead's score is the number of `9`-height positions reachable from the
trailhead via a hiking trail. What is the sum of the scores of all trailheads?
{{% cite AoC2024Day10 %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/10-hoof-it/HoofIt.PartOne.cs"
  highlight="cs"
  id="HoofIt.PartOne.cs" >}}

## Part Two

A trailhead's rating is the number of distinct hiking trails that begin at that
trailhead. What is the sum of the ratings of all trailheads? {{% cite
AoC2024Day10 %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/10-hoof-it/HoofIt.PartTwo.cs"
  highlight="cs"
  id="HoofIt.PartTwo.cs" >}}

## Counting Distinct Paths from TrailHead to TrailEnd

The grid traversal reminds of some LeetCode problems: {{% cite LCUniquePathsI
%}} and {{% cite LCUniquePathsII %}}. {{% cite LCUniquePathsI %}} traverses an
\\(R \times C\\) grid from the top left to the bottom right, moving either right
or down at each step. We need to move down \\(R - 1\\) times and move right \\(C
- 1\\) times, resulting in \\(R + C - 2\\) total moves. The number of possible
paths comes from choosing either when to go down or when to go right {{% cite
WikiLatticePaths %}}, i.e.,

$$ \binom{R + C - 2}{R - 1} = \binom{R + C - 2}{C - 1} $$

{{% cite LCUniquePathsII %}} imposes the same traversal rules as {{% cite
LCUniquePathsI %}}, but adds obstacles in the path. The combinatorics solution
no longer works, and so we fall back to the dynamic programming formulation:
`dp[r, c] = dp[r - 1, c] + dp[r, c - 1]`.

Compared to {{% cite LCUniquePathsII %}}, {{% cite AoC2024Day10 %}} has multiple
ending positions per origin, and can traverse left and up as well. It's not
clear to me how to formulate {{% cite AoC2024Day10 %}} as a dynamic programming
problem for better memory efficiency. This approach is correct and fast enough,
but we are creating a lot of intermediate arrays:

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/10-hoof-it/HoofIt.Common.cs"
  highlight="cs"
  id="HoofIt.Common.cs" >}}

## References

1. {{< citation
  id="AoC2024Day10"
  title="Day 10 - Advent of Code 2024: Hoof It"
  url="https://adventofcode.com/2024/day/10"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

1. {{< citation
  id="LCUniquePathsII"
  title="Unique Paths II - LeetCode"
  url="https://leetcode.com/problems/unique-paths-ii/description/"
  accessed="2025-11-28" >}}

1. {{< citation
  id="LCUniquePathsI"
  title="Unique Paths - LeetCode"
  url="https://leetcode.com/problems/unique-paths/description/"
  accessed="2025-11-28" >}}

1. {{< citation
  id="WikiLatticePaths"
  title="Lattice path - Wikipedia"
  url="https://en.wikipedia.org/wiki/Lattice_path"
  accessed="2025-11-28" >}}
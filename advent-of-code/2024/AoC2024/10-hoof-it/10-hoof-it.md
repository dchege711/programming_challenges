---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
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

## References

1. {{< citation
  id="AoC2024Day10"
  title="Day 10 - Advent of Code 2024: Hoof It"
  url="https://adventofcode.com/2024/day/10"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

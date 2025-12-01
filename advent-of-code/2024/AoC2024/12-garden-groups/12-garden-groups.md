---
cited-authors:
- Wastl, Eric
date: 2025-11-30
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/12-garden-groups/12-garden-groups/
title: 'AoC 2024 Day 12: Garden Groups'
---

## Parsing

Each garden plot grows only a single type of plant indicated by a single letter.
This \\(4 \times 4\\) arrangement includes garden plots growing 5 different
types of plants (labelled `A`, `B`, `C`, `D`, `E`). {{% cite AoC2024Day12 %}}

```txt
AAAA
BBCD
BBCC
EEEC
```

The <dfn>area</dfn> of a region is the number of garden plots the region
contains. The <dfn>perimeter</dfn> of a region is the number of sides of garden
plots in the region that do not touch another garden plot in the same region.
For example, the `C` plans are in a region with area \\(4\\) and perimeter
\\(8\\). {{% cite AoC2024Day12 %}}

```txt
+-+-+-+-+
|A A A A|
+-+-+-+-+     +-+
              |D|
+-+-+   +-+   +-+
|B B|   |C|
+   +   + +-+
|B B|   |C C|
+-+-+   +-+ +
          |C|
+-+-+-+   +-+
|E E E|
+-+-+-+
```

Plants of the same type can appear in multiple separate regions, and regions can
even appear within other regions, e.g.,

```txt
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
```

{{% cite AoC2024Day12 %}}

## Part One

The <dfn>price</dfn> of a fence required for a region is found by multiplying
that region's area by its perimeter. What is the total price of fencing all
regions on your map?

## References

1. {{< citation
  id="AoC2024Day12"
  author="Eric Wastl"
  title="Day 12 - Advent of Code 2024: Garden Groups"
  url="https://adventofcode.com/2024/day/12"
  accessed="2025-11-30" >}}

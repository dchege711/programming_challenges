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
For example, the `C` plants are in a region with area \\(4\\) and perimeter
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

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/12-garden-groups/GardenGroups.Parse.cs"
  highlight="cs"
  id="GardenGroups.Parse.cs" >}}

## Part One

The <dfn>price</dfn> of a fence required for a region is found by multiplying
that region's area by its perimeter. What is the total price of fencing all
regions on your map? {{% cite AoC2024Day12 %}}

Is it worthwhile to think of {{% cite AoC2024Day12 %}} in terms of connected
components, where each region is a connected component? The number of
sub-components in the connected component gives us the area. The perimeter is
tricky because we'd need to at least scan the ends of each row. I don't see how
computing connected components makes our work easier.

What if we start from the top-left and scan left to right? At each \\((r, c)\\),
we can expand out to cover the whole region. To avoid double-counting, we need a
second map to efficiently avoid already-counted areas. Otherwise, deducing that
\\((2, 0)\\) is in the same region as \\((0, 3)\\) could be a lot of repeated
work.

```txt
   0 1 2 3
        +-+
0       |A|
  +-+-+-+ +
1 |A A A A|
  +       +
2 |A A A A|
  +-+-+-+-+
```

For part one, the fact that \\(A\\) can have multiple separate regions doesn't
come into play. We'd get the same total price if each \\(A\\) region was
replaced by any other plant type. I suspect that part 2 of {{% cite AoC2024Day12
%}} might want to use this information.

How should a region be represented in a form amenable to computing area and
perimeter? Is there use in representing the region? It seems like the act of
computing a region also allows us to collect the area. If the region computation
is too busy, I'll separate out the area and perimeter calculations to their own
subroutines. In that case,

```txt
   0 1 2 3 4 5 6 7
  +-+-+       +-+-+
0 |H H|       |H H|
  + +-+       +-+ +
1 |H|           |H|
  + +-+-+-+-+-+-+ +
2 |H H H H H H H H|
  + +-+-+-+-+-+-+ +
3 |H|           |H|
  +-+           +-+
```

...can be represented by the column indices:

```json
[
  [[0, 1], [6, 7]],
  [[0, 0], [7, 7]],
  [[0, 7]],
  [[0, 0], [7, 7]]
]
```

... and a note that \\(r = 0\\) is the starting row, and `H` is the plant type.
That information is enough to recreate the original map. The above form is more
amenable to computing the area than to computing the perimeter.

On the other hand, a representation with a list of vertices in either clockwise
or counterclockwise order makes it convenient to compute the perimeter. I don't
know of a representation that supports area and perimeter computations
simultaneously. It seems easier to transform the column indices into a list of
vertices than the other way round.

Scratch that. Collecting the "collected component" using breadth first search
makes it trivial to compute the area during the BFS. BFS also makes it possible
to compute how much each cell will contribute to the perimeter based on its
neighbors.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/12-garden-groups/GardenGroups.PartOne.cs"
  highlight="cs"
  id="GardenGroups.PartOne.cs" >}}

## Part Two

Instead of using perimeter, use the number of sides that a region has. What is
the new total price of fencing all regions on your map? {{% cite AoC2024Day12
%}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/12-garden-groups/GardenGroups.PartTwo.cs"
  highlight="cs"
  id="GardenGroups.PartTwo.cs" >}}

Struggled a lot on this part until I looked at {{% cite rAOCDay12Part2 %}}. That
the number of corners equals the number of sides in a polygon simplified the
problem because it's easier to count the corners.

{{% comment %}}

Covered bijections in <em>COS 340: Reasoning about Computation</em> but I don't
think I had encountered their practicality in the world. In this case, the
bijection between corners and sides works a lot in my favor.

{{% /comment %}}

## Core Routines

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/12-garden-groups/GardenGroups.Common.cs"
  highlight="cs"
  id="GardenGroups.Common.cs" >}}

## References

1. {{< citation
  id="AoC2024Day12"
  author="Eric Wastl"
  title="Day 12 - Advent of Code 2024: Garden Groups"
  url="https://adventofcode.com/2024/day/12"
  accessed="2025-11-30" >}}

1. {{< citation
  id="rAOCDay12Part2"
  title="[2024 Day 12 (Part 2)] What kind of algorithm did you use on part 2? : r/adventofcode"
  url="https://www.reddit.com/r/adventofcode/comments/1hcpyic/2024_day_12_part_2_what_kind_of_algorithm_did_you/"
  accessed="2025-12-06" >}}

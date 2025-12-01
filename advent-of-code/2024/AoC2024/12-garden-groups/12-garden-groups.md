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
   0 1 2 3
        +-+
0       |A|
  +-+-+-+ +
1 |A A A A|
  +       +
2 |A A A A|
  +-+-+-+-+
```

.. can be represented by the column indices:

```json
[
  [3, 3]
  [0, 3]
  [0, 3]
]
```

... and a note that \\(r = 0\\) is the starting row, and `A` is the plant type.
That information is enough to recreate the original map.

## References

1. {{< citation
  id="AoC2024Day12"
  author="Eric Wastl"
  title="Day 12 - Advent of Code 2024: Garden Groups"
  url="https://adventofcode.com/2024/day/12"
  accessed="2025-11-30" >}}

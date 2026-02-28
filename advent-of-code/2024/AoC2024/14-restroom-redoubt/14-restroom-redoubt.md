---
cited-authors:
- Wastl, Eric
date: 2026-01-03
domains:
- adventofcode.com
- en.wikipedia.org
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/14-restroom-redoubt/14-restroom-redoubt/
title: 'AoC 2024 Day 14: Restroom Redoubt'
---

## Parsing

The input is a list of all robots' current positions \\(p = (x, y)\\) and
velocities \\(v = (dx, dy)\\), one robot per line, e.g.,

```txt
p=3,6 v=4,-7
p=9,2 v=-1,-2
```

{{% cite AoC2024Day14 %}}

\\(x\\) represents the number of tiles away from the left wall, and similarly
for \\(y\\) from the top wall (when viewed from above). The top-left corner of
the space is \\((0, 0)\\). The velocity is given in tiles per second. {{% cite
AoC2024Day14 %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/14-restroom-redoubt/RestroomRedoubt.DataTypes.cs"
  highlight="cs"
  id="RestroomRedoubt.DataTypes.cs" >}}

## Part One

The area of the region is \\(101\\) tiles wide and \\(103\\) tiles tall. When a
robot runs into an edge, they wrap around to the other side. Multiple robots can
be on the same tile without interference. {{% cite AoC2024Day14 %}}

Count the number of robots in each quadrant after 100s. Robots exactly in the
middle don't count as being in any quadrant. What is the product of the counts?
{{% cite AoC2024Day14 %}}

Some properties of the problem:

* The robots non-interference with each other makes this an embarrasingly
  parallel problem, where each robot's trajectory can be computed independently.
* A robot's trajectory can be computed in \\(\mathcal{O}(1)\\) time because the
  velocity is constant. Think Physics 101; even constant acceleration is still
  computable in \\(\mathcal{O}(1)\\) time.
* Wrapping around edges is equivalent to performing modular arithmetic.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/14-restroom-redoubt/RestroomRedoubt.PartOne.cs"
  highlight="cs"
  id="RestroomRedoubt.PartOne.cs" >}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/14-restroom-redoubt/RestroomRedoubt.Extensions.cs"
  highlight="cs"
  id="RestroomRedoubt.Extensions.cs" >}}

Modulo operator with negative numbers varies in different programming languages.
Implementing `is_odd` as `n % 2 == 1` would fail in a language where \\(-1 \mod
2 = -1\\); instead `n % 2 != 0` would work because remainder 0 is the same
regardless of signs. C# defines \\(-1 \mod 2 = -1\\)  {{% cite WikiModulo %}}.

## References

1. {{< citation
  id="AoC2024Day14"
  author="Eric Wastl"
  title="Day 14 - Advent of Code 2024"
  url="https://adventofcode.com/2024/day/14"
  accessed="2026-01-03" >}}

1. {{< citation
  id="WikiModulo"
  title="Modulo - Wikipedia"
  url="https://en.wikipedia.org/wiki/Modulo"
  accessed="2026-02-28" >}}

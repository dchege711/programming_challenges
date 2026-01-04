---
title: "AoC 2024 Day 14: Restroom Redoubt"
date: 2026-01-03
---

## Parsing

The input is a list of all robots' current positions \\(p = (x, y)\\) and
velocities \\(v = (dx, dy)\\), one robot per line, e.g.,

```txt
p=3,6 v=4,-7
p=9,2 v=-1,-2
```

{{% cite AoC2024Day14 %}}

In \\((x, y)\\), \\(x\\) represents the number of tiles away from the left wall,
and similarly for \\(y\\) from the top wall (when viewed from above). The
top-left corner of the space is \\((0, 0)\\). The velocity is given in tiles
per second. {{% cite AoC2024Day14 %}}

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

## References

1. {{< citation
  id="AoC2024Day14"
  author="Eric Wastl"
  title="Day 14 - Advent of Code 2024"
  url="https://adventofcode.com/2024/day/14"
  accessed="2026-01-03" >}}
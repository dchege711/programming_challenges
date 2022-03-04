---
date: 2022-02-27
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/HydrothermalVenture/05-hydrothermal-venture/
title: 'AoC 2021 Day 05: Hydrothermal Venture'
weight: 5
---

## Problem Statement

### Part One

You come across a field of **hydrothermal vents** on the ocean floor!
These vents constantly produce large, opaque clouds, so it would be best
to avoid them if possible.

They tend to form in **lines**; the submarine hopefully produces a list
of nearby lines of vents (your puzzle input) for you to review.

Each line of vents is given as a line segment in the format `(x1, y1 ->
x2, y2)`, where `x1,y1` are the coordinates of one end of the line
segment, and `x2,y2` are the coordinates of the other end. These line
segments include the points at both ends. In other words:

* An entry like `1,2 -> 1,3` covers points `1,1`, `1,2`, and `1,3`.
* An entry like `9,7 -> 7,7` covers points `9,7`, `8,7`, and `7,7`.

For now, **only consider horizontal and vertical lines:** lines where
either `x1 == x2` or `y1 == y2`.

To avoid the most dangerous areas, you need to determine **the number of
points where at least two lines overlap**. **At how many points do at
least two lines overlap?**

### Part Two

Unfortunately, considering only horizontal and vertical lines doesn't
give you the full picture; you need to also consider **diagonal lines**.

Because of the limits of the hydrothermal vent mapping system, the lines
in your list will only ever be horizontal, vertical, or a diagonal line
at exactly 45 degrees. In other words:

* An entry like `1,1 -> 3,3` covers points `1,1`, `2,2`, and `3,3`.
* An entry like `9,7 -> 7.9` covers points `9,7`, `8,8`, and `7,9`.

Consider all of the lines. **At how many points do at least two lines
overlap?**

## My Solution

{{< readfile
  file=`content/computer-science/programming-challenges/advent-of-code/2021/src/HydrothermalVenture/HydrothermalVenture.hs`
  highlight="haskell"
  id="HydrothermalVenture.hs" >}}

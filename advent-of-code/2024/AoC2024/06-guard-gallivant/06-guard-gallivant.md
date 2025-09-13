---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
- github.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/06-guard-gallivant/06-guard-gallivant/
title: 'AoC 2024 Day 06: Guard Gallivant'
---

## Parsing

The map shows the current position of the guard with `^` (to indicate the guard
is facing **up** from the perspective of the map). Any **obstructions** -
crates, desks, alchemical reactors, etc., are shown as `#`.

<table>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>#</td><td>.</td><td>.</td><td>^</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td></tr>
<tr><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td></tr>
</table>

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/06-guard-gallivant/GuardGallivant.Parse.cs"
  highlight="cs"
  id="GuardGallivant.Parse.cs" >}}

## Part One

The guard follows a strict patrol protocol which involves repeatedly following
these steps:

* If there is something directly in front of you, turn right 90 degrees.
* Otherwise, take a step forward.

How many distinct positions will the guard visit before leaving the mapped area?

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/06-guard-gallivant/GuardGallivant.PartOne.cs"
  highlight="cs"
  id="GuardGallivant.PartOne.cs" >}}

## Part Two

How many different positions can you choose for adding a new obstruction that
gets the guard stuck in a loop? The new obstruction can't be placed at the
guard's starting position.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/06-guard-gallivant/GuardGallivant.PartTwo.cs"
  highlight="cs"
  id="GuardGallivant.PartTwo.cs" >}}

The performance of this step is noteworthy. At first, I used `PositionState[,]`,
a 2D array, to represent the area map. I solved by creating a new 2D array with
a new obstacle at \\((r, c)\\) slotted in. This copied a lot of data around, and
the test took 20s to run. {{% cite programming_challenges-56c85e9 %}} moved from
`PositionState[,]` to `(int RowCount, int ColCount, HashSet<Coordinate>
Obstacles) AreaMap`, which is a more memory efficient representation. To solve
part 2, I mutated `HashSet<Coordinate> Obstacles` in place, but that took 25s to
run! Aha, no need to place obstacles at locations where the guard will never
visit; that takes the runtime down from 25s to 3s! Hindsight is 20/20.

## Reference

1. {{< citation
  id="AoC2024Day06"
  title="Day 06 - Advent of Code 2024: Guard Gallivant"
  url="https://adventofcode.com/2024/day/6"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

1. {{< citation
  id="programming_challenges-56c85e9"
  title="[AoC 2024] [Guard Gallivant] Use sparse representation of map · dchege711/programming_challenges@56c85e9"
  url="https://github.com/dchege711/programming_challenges/commit/56c85e9ae0ca04f40b3155a83086dd8953694cba#diff-93422f2e2791d2fc730af4949784390b98b88acc23648e9e0c4751e416eda913"
  date="2025-09-11"
  accessed="2025-09-11" >}}

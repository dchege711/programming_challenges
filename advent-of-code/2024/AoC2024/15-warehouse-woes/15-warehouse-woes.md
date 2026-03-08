---
cited-authors:
- Wastl, Eric
date: 2026-02-28
domains:
- adventofcode.com
- softwareengineering.stackexchange.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/15-warehouse-woes/15-warehouse-woes/
title: 'AoC 2024 Day 15: Warehouse Woes'
---

## Parsing

```txt
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
v^^>>><<v^^>>>^
```

`@` denotes the robot, `O` denotes a box, and `#` denotes a wall.
`<^^>>>vv<v>>v<<` describes the sequence of moves that the robot will attempt to
make. Ignore the newlines within the move sequence. If there are any boxes in
the way, the robot attempts to push them. However, if the action makes the robot
or a box move into the wall, nothing moves. {{% cite AoC2024Day15 %}}

How should the \\(R \times C\\) region be represented? If the walls and boxes
are scarce, then a map representation will be memory efficient. The test input
is \\(50 \times 50\\) with \\(396\\) walls and \\(613\\) boxes, and thus
\\(59.64\\%\\) empty.

Given a move, I need to look up and update the \\(R \times C\\) region in at
most \\(R-2\\) or \\(C-2\\) cells. These lookups and updates need to be fast. A
2D array will have good cache locality especially when processing lateral moves.

## Part One

The GPS coordinate of a box is equal to \\(100\\) times its distance from the
top edge of a map plus its distance from the left edge of the map. After the
robot is finished moving, what is the sum of all boxes' GPS coordinates? {{%
cite AoC2024Day15 %}}

## Part Two

Everything except the robot is twice as wide: `#` -> `##`, `O` -> `[]`, `.` ->
`..`, and `@` -> `@.`. For these larger boxes, distances are measured from the
edge of the map to the closest edge of the box in question. What is the sum of
all boxes' final GPS coordinates? {{% cite AoC2024Day15 %}}

## Solution

There are some stateless operations that can be solved first and independently
in `WarehouseWoes.Extensions.cs` without considering any algorithms:

* Convert a direction, e.g., `^` to a \\((dr, dc)\\) vector.
* Given a coordinate \\((r, c)\\), add/subtract \\((dr, dc)\\) to/from it.
* Test if a coordinate \\((r, c)\\) is in bounds.
* Tell if a direction is lateral (`<`, `>`) or medial (`^`, `v`).

<details>
<summary>WarehouseWoes.Extensions.cs</summary>

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/15-warehouse-woes/WarehouseWoes.Extensions.cs"
  highlight="cs"
  id="WarehouseWoes.Extensions.cs" >}}

</details>

Compared to [Part One](#part-one), [Part Two](#part-two)'s `Move` operation is
more complicated because `[` and `]` need to move as a unit. Furthermore, there
is a domino effect, e.g., `^` doesn't succeed because the top-right `]` can't
move:

```txt
##############
##......##..##
##.....[]...##
##....[]....##
##.....@....##
##..........##
##############
```

*Data is more tractable than program logic. It follows that where you see a
choice between complexity in data structures and complexity in code, choose the
former. More: in evolving a design, you should actively seek ways to shift
complexity from code to data.* - The Art of Unix Programming. {{% cite SE163185
%}} I had a costly detour away from the 2D array with the desire to make it
impossible to move `[` and `]` independently. However, even though the data
structure collapsed `[]` into a single C# `record`, the algorithm was gnarly and
still performing a lot of vector arithmetic. Keeping the 2D array and then
simplifying the algorithm worked better here. Remember, the goal is to reduce
overall complexity.

<figure>
    <img
      width="100%"
      src='/img/computer-science/programming-challenges/advent-of-code/2024/day-13-sample-9021-buggy.gif'
      alt='Buggy solution for part 2.'
      loading="lazy">
    <figcaption>
      Buggy solution for part 2. Visualizing the result helped me spot and <a
        href="https://github.com/dchege711/programming_challenges/commit/164062cbf0d55bd81c8ead51a240d940a052f713">
      fix the zigzag pattern</a>. <code>GPT 5.3-Codex</code> wrote this script
      using the <code>SixLabors.ImageSharp</code> pattern.
    </figcaption>
</figure>

<details>
<summary>WarehouseWoes.Common.cs</summary>

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/15-warehouse-woes/WarehouseWoes.Common.cs"
  highlight="cs"
  id="WarehouseWoes.Common.cs" >}}

</details>

## References

1. {{< citation
  id="AoC2024Day15"
  author="Eric Wastl"
  title="Day 15 - Advent of Code 2024: Warehouse Woes"
  url="https://adventofcode.com/2024/day/15"
  accessed="2026-02-28" >}}

1. {{< citation
  id="SE163185"
  title="Torvalds' quote about good programmer [closed]"
  url="https://softwareengineering.stackexchange.com/questions/163185/torvalds-quote-about-good-programmer"
  accessed="2026-03-01" >}}

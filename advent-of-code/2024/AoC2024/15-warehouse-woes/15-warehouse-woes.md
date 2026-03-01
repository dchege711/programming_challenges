---
cited-authors:
- Wastl, Eric
date: 2026-02-28
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/14-restroom-redoubt/14-restroom-redoubt/
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
2D array will have good cache locality especially when processing `<`s and `>`s.
Will go with a 2D array.

## Part One

The GPS coordinate of a box is equal to \\(100\\) times its distance from the
top edge of a map plus its distance from the left edge of the map. After the
robot is finished moving, what is the sum of all boxes' GPS coordinates? {{%
cite AoC2024Day15 %}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/15-warehouse-woes/WarehouseWoes.PartOne.cs"
  highlight="cs"
  id="WarehouseWoes.PartOne.cs" >}}

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/15-warehouse-woes/WarehouseWoes.Extensions.cs"
  highlight="cs"
  id="WarehouseWoes.Extensions.cs" >}}

## Part Two

Everything except the robot is twice as wide: `#` -> `##`, `O` -> `[]`, `.` ->
`..`, and `@` -> `@.`. For these larger boxes, distances are measured from the
edge of the map to the closest edge of the box in question. What is the sum of
all boxes' final GPS coordinates? {{% cite AoC2024Day15 %}}

How much of [Part One](#part-one) still applies? We now need to consider two
cells at a time, e.g., need to move `[]` together. Instead of `CellType.Box`, we
could have `CellType.BoxStart` and `CellType.BoxEnd`. GPS coordinates can be
measured in terms of `CellType.BoxStart`. I think there's promise here; let's
see how it looks.

## References

1. {{< citation
  id="AoC2024Day15"
  author="Eric Wastl"
  title="Day 15 - Advent of Code 2024: Warehouse Woes"
  url="https://adventofcode.com/2024/day/15"
  accessed="2026-02-28" >}}

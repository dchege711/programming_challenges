---
cited-authors:
- Wastl, Eric
date: 2026-03-09
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/16-reindeer-maze/16-reindeer-maze/
title: 'AoC 2024 Day 16: Reindeer Maze'
---

## Data Parsing

The input is an \\(R \times C\\) grid with `#` for a wall, `S` for the start
tile, `E` for the end tile, and `.` for open spots. {{% cite AoC2024Day16 %}}

## Part One

The reindeer start at `S` facing `East`, and can move one tile at a time,
increasing their score by `1` point. They can also rotate clockwise or
counterclockwise 90 degrees at a time, increasing their score by `1000` points.
What is the lowest score a Reindeer could possibly get? {{% cite AoC2024Day16
%}}

This is a [shortest paths problem]({{< ref
"/computer-science/algorithms-and-data-structures/graphs/shortest-paths" >}})
where Dijkstra's algorithm is appropriate. The nodes are tiles on the grid. The
cost of an edge varies: if the next tile is in the same direction as we're
currently heading, then the cost is `1`, otherwise, the cost is `1000 + 1`.

## References

1. {{< citation
  id="AoC2024Day16"
  author="Eric Wastl"
  title="Day 16 - Advent of Code 2024"
  url="https://adventofcode.com/2024/day/16"
  accessed="2026-03-09" >}}

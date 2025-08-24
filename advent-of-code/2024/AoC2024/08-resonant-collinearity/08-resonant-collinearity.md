---
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/08-resonant-collinearity/08-resonant-collinearity/
title: 'AoC 2024 Day 08: Resonant Collinearity'
---

## Problem Statement

### Part One

Scanning across the city, you find that there are actually many such antennas.
Each antenna is tuned to a specific **frequency** indicated by a single
lowercase letter, uppercase letter, or digit. You create a map of these
antennas:

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024.Tests/data/day-08-sample.in.txt"
  highlight="txt" >}}

The signal only applies its nefarious effect at specific **antinodes** based on
the resonant frequencies of the antennas. In particular, an antinode occurs at
any point that is perfectly in line with two antennas of the same frequency --
but only when one of the antennas is twice as far away as the other. This means
that for any pair of antennas with the same frequency, there are two antinodes,
one on either side of them.

So, for these two antennas with frequency `a`, they create the antinodes marked
with `#`:

```txt
..........
...#......
..........
....a.....
..........
.....a....
..........
......#...
..........
..........
```

Adding a third antenna with the same frequency creates several more antinodes.
It would ideally add four antinodes, but two are off the right side of the map,
so instead it adds only two:

```txt
..........
...#......
#.........
....a.....
........a.
.....a....
..#.......
......#...
..........
..........
```

Antennas with different frequencies don't create antinodes; `A` and `a` count as
different frequencies. However, antinodes **can** occur at locations that contain
antennas.

Calculate the impact of the signal. **How many unique locations within the
bounds of the map contain an antinode?**

1. {{< citation
  id="AoC2024Day08"
  title="Day 08 - Advent of Code 2024: Resonant Collinearity"
  url="https://adventofcode.com/2024/day/8"
  accessed="2025-08-23" >}}

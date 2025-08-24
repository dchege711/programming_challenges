---
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/06-guard-gallivant/06-guard-gallivant/
title: 'AoC 2024 Day 06: Guard Gallivant'
---

## Problem Statement

### Part One

You still have to be careful of time paradoxes, and so it will be important to
avoid anyone from 1518 while the Historians search for the Chief. Unfortunately,
a single **guard** is controlling this part of the lab.

Maybe you can work out where the guard will go ahead of time so that The
Historians can search safely?

You start by making a map of the situation. For example:

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024.Tests/data/day-06-sample.in.txt"
  highlight="txt" >}}

The map shows the current position of the guard with `^` (to indicate the guard
is facing **up** from the perspective of the map). Any **obstructions** -
crates, desks, alchemical reactors, etc., are shown as `#`.

Lab guards in 1518 follow a very strict patrol protocol which involves
repeatedly following these steps:

* If there is something directly in front of you, turn right 90 degrees.
* Otherwise, take a step forward.

By predicting the guard's route, you can determine which specific positions in
the lab will be in the patrol path. **Including the guard's starting position,**
the positions visited by the guard before leaving the area are marked with an
`X`:

```txt
....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..
```

In this example, the guard will visit `41` distinct positions on your map.

Predict the path of the guard. **How many distinct positions will the guard
visit before leaving the mapped area?**

1. {{< citation
  id="AoC2024Day06"
  title="Day 06 - Advent of Code 2024: Guard Gallivant"
  url="https://adventofcode.com/2024/day/6"
  accessed="2025-08-23" >}}

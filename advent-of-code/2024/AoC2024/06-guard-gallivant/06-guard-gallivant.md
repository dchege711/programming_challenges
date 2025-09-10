---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
draft: true
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

## Part One

The guard follows a strict patrol protocol which involves repeatedly following
these steps:

* If there is something directly in front of you, turn right 90 degrees.
* Otherwise, take a step forward.

How many distinct positions will the guard visit before leaving the mapped area?

## Reference

1. {{< citation
  id="AoC2024Day06"
  title="Day 06 - Advent of Code 2024: Guard Gallivant"
  url="https://adventofcode.com/2024/day/6"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

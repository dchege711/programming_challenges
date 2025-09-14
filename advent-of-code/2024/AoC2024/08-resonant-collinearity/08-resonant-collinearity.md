---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/08-resonant-collinearity/08-resonant-collinearity/
title: 'AoC 2024 Day 08: Resonant Collinearity'
---

## Parsing

Each antenna in the map is tuned to a frequency indicated by a single lowercase
letter, uppercase letter, or digit. {{% cite AoC2024Day08 %}}

Two antennas with the same frequency create two collinear antinodes on either
side where one of the antennas is twice as far away as the other, e.g.

<table>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>a</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>a</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
</table>

Antinodes can occur at locations that contain other antennas. {{% cite
AoC2024Day08 %}}

## Part One

How many unique locations within the bounds of the map contain an antinode? {{%
cite AoC2024Day08 %}}

1. {{< citation
  id="AoC2024Day08"
  title="Day 08 - Advent of Code 2024: Resonant Collinearity"
  url="https://adventofcode.com/2024/day/8"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

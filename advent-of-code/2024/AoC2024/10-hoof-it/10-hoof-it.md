---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/10-hoof-it/10-hoof-it/
title: 'AoC 2024 Day 10: Hoof It'
---

## Problem Statement

### Part One

The topographic map indicates the **height** at each position using a scale from
`0` (lowest) to `9` (highest):

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024.Tests/data/day-10-sample.in.txt"
  highlight="txt" >}}

Based on un-scorched scraps of the book, you determine that a good hiking trail
is **as long as possible** and has an **even, gradual, uphill slope**. For all
practical purposes, this means that a hiking trail is any path that starts at
height `0`, ends at height `9`, and always increases by a height of exactly `1`
at each step. Hiking trails never include diagonal steps - only up, down, left,
or right (from the perspective of the map).

A **trailhead** is any position that starts one or more hiking trails - here,
these positions will always have height `0`. Assembling more fragments of pages,
you establish that a trailhead's **score** is the number of 9-height positions
reachable from that trailhead via a hiking trail. In the above example, the
single trailhead in the top left corner has a score of `1` because it can reach
a single `9` (the one in the bottom left).

**What is the sum of the scores of all trailheads on your topographical map?**

1. {{< citation
  id="AoC2024Day10"
  title="Day 10 - Advent of Code 2024: Hoof It"
  url="https://adventofcode.com/2024/day/10"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

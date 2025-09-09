---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/07-bridge-repair/07-bridge-repair/
title: 'AoC 2024 Day 07: Bridge Repair'
---

## Problem Statement

### Part One

You ask how long it'll take to fix the bridge; the engineers tell you that it
only needs final calibrations, but some young elephants were playing nearby and
**stole all the operators** from their calibration equations! They could finish
the calibrations if only someone could determine which test values could
possibly be produced by placing any combination of their operators into their
calibration equations.

For example:

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024.Tests/data/day-07-sample.in.txt"
  highlight="txt" >}}

Each line represents a single equation. The test value appears before the colon
on each line; it is your job to determine whether the remaining numbers can be
combined with operators to produce the test value.

Operators are **always evaluated left-to-right**, **not** according to
precedence rules. Furthermore, numbers in the equations cannot be rearranged.
Glancing into the jungle, you can see elephants holding two different types of
operators: **add** and **multiply**.

Only three of the above equations can be made true by inserting operators:

* \\(190 = 10 \times 19\\)
* \\(3267 = (81 + 40) \times 27\\) or \\(3267 = (81 * 40) + 27\\)
* \\(292 = ((11 + 6) \times 16) + 20\\)

The engineers just need the **total calibration result**, which is the sum of
the test values from just the equations that could possibly be true. In the
above example, the sum of the test values for the three equations listed above
is `3749`.

Determine which equations could possibly be true. **What is their total
calibration result?**

1. {{< citation
  id="AoC2024Day07"
  title="Day 07 - Advent of Code 2024: Bridge Repair"
  url="https://adventofcode.com/2024/day/7"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

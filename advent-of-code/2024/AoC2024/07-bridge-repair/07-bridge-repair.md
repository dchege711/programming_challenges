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

## Parsing

Each line represents a single equation, e.g., `292: 11 6 16 20`. [Part
One](#part-one) needs to make a decision based on each line independently.
Parsing each line into a data structure and yielding that should suffice.

## Part One

Using only **add** and **multiply**, evaluated left-to-right (not according to
math precedence rules), determine which equations could possibly be true. For
example, \\(292 = ((11 + 6) \times 16) + 20\\).

Of the equations that could possibly be true, what is the sum of their results?

## References

1. {{< citation
  id="AoC2024Day07"
  title="Day 07 - Advent of Code 2024: Bridge Repair"
  url="https://adventofcode.com/2024/day/7"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

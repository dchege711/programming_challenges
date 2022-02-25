---
date: 2022-02-25
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/GiantSquid/04-giant-squid/
title: 'AoC 2021 Day 04: Giant Squid'
weight: 4
---

{{< citation
  id="AoC2021-04"
  title="Day 4 - Advent of Code 2021"
  url="https://adventofcode.com/2021/day/4"
  accessed="2022-02-25" >}}

## Problem Statement

You're already almost 1.5km (almost a mile) below the surface of the
ocean, already so deep that you can't see any sunlight. What you **can**
see, however, is a giant squid that has attached itself to the outside
of your submarine.

Maybe it wants to play
[bingo](https://en.wikipedia.org/wiki/Bingo_(American_version))?

Bingo is played on a set of boards each consisting of a 5x5 grid of
numbers. Numbers are chosen at random, and the chosen number is
**marked** on all boards on which it appears. (Numbers may not appear on
all boards.) If all numbers in any row or any column of a board are
marked, that board **wins**. (Diagonals don't count.)

The submarine has a **bingo subsystem** to help passengers (currently,
you and the giant squid) pass the time. It automatically generates a
random order in which to draw numbers and a random set of boards (your
puzzle input).

The **score** of the winning board can be calculated. Start by finding
the **sum of all unmarked numbers** on that board. Then multiply that
sum by **the number that was just called** when the board won, to get
the final score.

To guarantee victory against the giant squid, figure out which board
will win first. **What will your final score be if you choose that
board?**

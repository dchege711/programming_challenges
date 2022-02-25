---
date: 2022-02-23
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/03-binary-diagnostic/
title: 'AoC 2021 Day 03: Binary Diagnostic'
weight: 3
---

{{< citation
    id="AoC2021-03"
    title="Day 3 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/3"
    accessed="2022-02-23" >}}

## Problem Description

### Part One

The submarine has been making some odd creaking noises, so you ask it to
produce a diagnostic report just in case.

The diagnostic report (your puzzle input) consists of a list of binary
numbers which, when decoded properly, can tell you many useful things
about the conditions of the submarine. The first parameter to check is
the **power consumption**.

You need to use the binary numbers in the diagnostic report to generate
two new binary numbers (called the **gamma rate** and the **epsilon
rate**). The power consumption can then be found by multiplying the
gamma rate by the epsilon rate.

Each bit in the gamma rate can be determined by finding the **most
common bit in the corresponding position** of all numbers in the
diagnostic report.

The epsilon rate is calculated in a similar way; rather than use the
most common bit, the least common bit from each position is used.

Use the binary numbers in your diagnostic report to calculate the gamma
rate and epsilon rate, then multiply them together. **What is the power
consumption of the submarine?** (Be sure to represent your answer in
decimal, not binary.)

{{% comment %}}

I don't see a fault in part 1. I anticipate that part II will involve
more bit manipulation in the name of getting more measurements beyond
power consumption. Update: That guess was correct.

{{% /comment %}}

### Part Two

Next, you should verify the **life support rating**, which can
determined by multiplying the **oxygen generator rating** by the
**CO2 scrubber rating**.

Both the oxygen generator rating and the CO2 scrubber rating are values
that can be found on your diagnostic report - finding them is the tricky
part. Both values are located using a similar process that involves
filtering out values until one remains. Before searching for either
rating value, start with the full list of binary numbers from your
diagnostic report and **consider just the first bit** of those numbers.
Then:

* Keep only numbers selected by the **bit criteria** for the type of
  rating value for which you are searching. Discard numbers which do not
  match the bit criteria.
* If you only have one number left, stop; this is the rating value for
  which you are searching.
* Otherwise, repeat the process, considering the next bit to the right.

The **bit criteria** depends on which type of rating value you want to
find:

* To find **oxygen generator rating**, determine the **most common**
  value (`0` or `1`) in the current bit position, and only keep the
  numbers with that bit in that position. If `0` and `1` are equally
  common, keep values with a `1` in the position being considered.
* To find **CO2 scrubber rating**, determine the **least common** value
  (`0` or `1`) in the current bit position, and only keep the numbers
  with that bit in that position. If `0` and `1` are equally common,
  keep values with a `0` in the position being considered.

Use the binary numbers in your diagnostic report to calculate the oxygen
generator rating and CO2 scrubber rating, then multiply then together.
**What is the life support rating of the submarine?** (Be sure to
represent your answer in decimal, not binary.)

## My Solution

{{< readfile
    file="/content/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/BinaryDiagnostic.hs"
    highlight="haskell"
    id="BinaryDiagnostic.hs" >}}

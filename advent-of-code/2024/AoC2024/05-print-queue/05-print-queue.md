---
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/05-print-queue/05-print-queue/
title: 'AoC 2024 Day 05: Print Queue'
---

## Problem Statement

### Part One

Safety protocols clearly indicate that new pages for the safety manuals must be
printed in a **very specific order.** The notation `X|Y` means that if both page
number `X` and page number `Y` are to be produced as part of an update, page
number `X` **must** be printed at some point before page number `Y`.

The Elf has for you both the **page ordering rules** and the **pages to produce
in each update** (your puzzle input), but can't figure out whether each update
has the pages in the right order.

For example:

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024.Tests/data/day-05-test.in.txt"
  highlight="txt" >}}

To get the printers going as soon as possible, start by identifying **which
updates are already in the right order.** In the example above, the first update
`(75, 47, 61, 53, 29)` is in the right order:

* `75` is correctly first because there are rules that put each other page after
  it: `75|47`, `75|61`, `75|53`, and `75|29`.
* `47` is correctly second because `75` must be before it (`75|47`) and every
  other page must be after it according to `47|61`, `47|53`, and `47|29`.
* `61` is correctly in the middle because `75` and `47` are before it (`75|61`
  and `47|61`) and `53` and `29` are after it (`61|53` and `61|29`).
* `53` is correctly fourth because it is before page number `29` (`53|29`).
* `29` is the only page left and so is correctly last.

Because the first update does not include some page numbers, the ordering rules
involving those missing page numbers are ignored.

The fourth update, `75,97,47,61,53`, is **not** in the correct order: it would
print `75` before `97`, which violates the rule `97|75`.

For some reason, the Elves also need to know the **middle page number** of each
update being printed. Because you are currently only printing the
correctly-ordered updates, you will need to find the middle page number of each
correctly-ordered update. In the above example, the correctly-ordered updates
are:

```txt
75,47,61,53,29
97,61,53,29,13
75,29,13
```

These have middle page numbers of `61`, `53`, and `29` respectively. Adding
these page numbers together gives `143`.

Determine which updates are already in the correct order. **What do you get if
you add up the middle page number from those correctly-ordered updates?**

1. {{< citation
  id="AoC2024Day05"
  title="Day 5 - Advent of Code 2024: Print Queue"
  url="https://adventofcode.com/2024/day/5"
  accessed="2025-08-23" >}}

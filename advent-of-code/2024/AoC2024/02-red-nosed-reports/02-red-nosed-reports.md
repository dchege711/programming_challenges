---
date: 2025-06-30
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/02-red-nosed-reports/02-red-nosed-reports/
title: 'AoC 2024 Day 02: Red-Nosed Reports'
---

## Data

The unusual data consists of many **reports**, one report per line. Each report
is a list of numbers called **levels** that are separated by spaces. For
example:

```txt
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
```

To parse:

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/02-red-nosed-reports/RedNosedReports.Parse.cs"
  highlight="cs"
  id="RedNosedReports.Parse.cs" >}}

## Part One

The Red-Nosed reactor safety systems can only tolerate levels that are either
gradually increasing or gradually decreasing. So, a report only counts as safe
if both of the following are true:

* The levels are either **all increasing** or **all decreasing**.
* Any two adjacent levels that differ by **at least one** and **at most three**.

In the example above, `7 6 4 2 1` is **safe** because the levels are all
decreasing by 1 or 2. `1 2 7 8 9` is **unsafe** because `2 7` is an increase of
`1`. In this example, 2 reports are **safe**.

Analyze the unusual data from the engineers. **How many reports are safe?**

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/02-red-nosed-reports/RedNosedReports.PartOne.cs"
  highlight="cs"
  id="RedNosedReports.PartOne.cs" >}}

## Part Two

The Problem Dampener is a reactor-mounted module that lets the reactor safety
systems **tolerate a single bad level** in what would otherwise be a safe
report. It's like the bad level never happened!

Now, the same rules apply as before, except if removing a single level from an
unsafe report would make it safe, the report instead counts as safe. In the
example above, `8 6 4 4 1` is **safe** by removing the third level (4).

Update your analysis by handling situations where the Problem Dampener can
remove a single level from the unsafe reports. **How many reports are now
safe?**

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/02-red-nosed-reports/RedNosedReports.PartTwo.cs"
  highlight="cs"
  id="RedNosedReports.PartTwo.cs" >}}

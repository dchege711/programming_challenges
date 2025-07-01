---
date: 2025-06-25
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/Day01/01/
title: 'AoC 2024 Day 01: Historian Hysteria'
---

{{< citation
  id="AoC2024Day01"
  title="Day 1 - Advent of Code 2024: Historian Hysteria"
  url="https://adventofcode.com/2024/day/1"
  accessed="2025-06-25" >}}

## Problem Statement

### Part One

Through out the Chief Historian's office, the historically significant locations
are listed not by the name, but by a unique number called the **location ID**.
To make sure they don't miss anything, The Historians split into two groups,
each searching the office and trying to create their own complete list of
location IDs.

There's just one problem: by holding the two lists up **side by side** (your
puzzle input), it quickly becomes clear that the lists aren't very similar.
Maybe you can help The Historians reconcile their lists?

For example:

```txt
3 4
4 3
2 5
1 3
3 9
3 3
```

Maybe the lists are only off by a small amount! To find out, pair up the numbers
and measure how far apart they are. Pair up the **smallest number in the left
list** with the **smallest number in the right list,** then the
**second-smallest left number** with the **second-smallest right number**, and
so on.

Within each pair, figure out **how far apart** the two numbers are; you'll need
to **add up all of those distances.** For example, if you pair up a 3 from the
left list with a 7 from the right list, the distance apart is 4; if you pair up
a 9 with a 3, the distance apart is 6.

To find the **total distance** between the left list and the right list, add up
the distances between all of the pairs you found.

Your actual left and right lists contain many location IDs. **What is the total
distance between your lists?**

### Part Two

Your analysis only confirmed what everyone feared: the two lists of location IDs
are indeed very different.

Or are they?

The Historians can't agree on which group made the mistakes **or** how to read
most of the Chief's handwriting, but in the commotion you notice an interesting
detail: a lot of location IDs appear in both lists! Maybe the other numbers
aren't location IDs at all but rather misinterpreted handwriting.

This time, you'll need to figure out exactly how often each number from the left
list appears in the right list. Calculate a total **similarity score** by adding
up each number in the left list after multiplying it by the number of times that
number appears in the right list.

For example, the first number in the left list is 3. It appears in the right
list 3 times, so the similarity score increases by //(3 \times 3 = 9//). The
third number in the left list is 2. It does not appear in the right list once,
so the similarity score does not increase (\\(2 \times 0 = 0\\)). For the
example list, the similarity score at the end of this process is \\(9 + 4 + 0 +
0 + 9 + 9 = 31\\).

Once again, consider your left and right lists. **What is their similarity
score?**

## My Solution

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/01-historian-hysteria/HistorianHysteria.cs"
  highlight="cs"
  id="HistorianHysteria.cs">}}

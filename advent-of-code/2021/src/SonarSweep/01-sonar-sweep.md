---
authors:
- Hidding, Johan
- Le, Justin
date: 2022-02-18
domains:
- adventofcode.com
- github.com
- jhidding.github.io
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/SonarSweep/01-sonar-sweep/
title: 'AoC 2021 Day 01: Sonar Sweep'
weight: 1
---

{{< citation
  id="AoC2021-01"
  title="Day 1 - Advent of Code 2021"
  url="https://adventofcode.com/2021/day/1"
  accessed="2022-02-18" >}}

## Problem Statement

### Part One

As the submarine drops below the surface of the ocean, it automatically
performs a sonar sweep of the nearby sea floor. On a small screen, the
sonar weep report (your puzzle input) appears: each line is a
measurement of the sea floor depth as the sweep looks further and
further away from the submarine.

For example, suppose you had the following report:

```md
199
200
208
210
200
207
240
269
260
263
```

This report indicates that, scanning outward from the submarine, the
sonar sweep found depths of 199, 200, 208, 210, and so on.

The first order of business is to figure out how quickly the depth
increases, just so you know what you're dealing with - you never know if
the keys will get carried into deeper water by an ocean current or a
fish or something.

To do this, count **the number of times a depth measurement increases**
from the previous measurement. (There is no measurement before the
first measurement.) In the example above, the changes are as follows:

```md
199 (N/A - no previous measurement)
200 (**increased**)
208 (**increased**)
210 (**increased**)
200 (decreased)
207 (**increased**)
240 (**increased**)
269 (**increased**)
260 (decreased)
263 (**increased**)
```

In this example, there are 7 measurements that are larger than the
previous measurement.

**How many measurements are larger than the previous measurement?**

To begin, [get your puzzle
input](https://adventofcode.com/2021/day/1/input).

### Part Two

Considering every single measurement isn't as useful as you expected:
there's just too much noise in the data.

{{% comment %}}

I think going forward, it'll be useful for me to guess what part two of
the problem will be, and see how my guess holds up.

In this case, taking rolling statistics is a technique for smoothening
out noise.

{{% /comment %}}

Instead, consider sums of a **three-measurement sliding window**. Again,
considering the above example:

```md
199 A
200 A B
208 A B C
210   B C D
200 E   C D
207 E F   D
240 E F G
269   F G H
260     G H
263       H
```

Start by comparing the first and second three-measurement windows. The
measurements in the first window are marked `A (199, 200, 208)`; their
sum is \\(199 + 200 + 208 = 607\\). The second window is marked `B (200,
208, 210`); its sum is \\(618\\). The sum of measurements in the second
window is larger than the sum of the first, so this first comparison
**increased**.

Your goal now is to count **the number of times the sum of measurements
in this sliding window increases** from the previous sum. So, compare
`A` with `B`, then compare `B` with `C`, then `C` with `D`, and so on.
Stop when there aren't enough measurements left to create a new
three-measurement sum.

In the above example, the sum of each three-measurement window is as
follows:

```md
A: 607 (N/A - no previous sum)
B: 618 (**increased**)
C: 618 (no change)
D: 617 (decreased)
E: 647 (**increased**)
F: 716 (**increased**)
G: 769 (**increased**)
H: 792 (**increased**)
```

In this example, there are `5` sums that are larger than the previous
sum.

Consider sums of a three-measurement sliding window. **How many sums are
larger than the previous sum?**

## My Solution

{{< readfile
  file=`content/computer-science/programming-challenges/advent-of-code/2021/src/SonarSweep/SonarSweep.hs`
  highlight="haskell"
  id="SonarSweep.hs" >}}

Notable concepts: pattern matching on lists, working with `Maybe`
values, lazy I/O.

## Learning from Others' Solutions

Without the parsing of the `[String]` into an `[Int]`, my solution is
basically:

```hs
numIncreases :: [Int] -> Int
numIncreases (x:y:zs) = total where
  contribution = if y > x then (1 :: Int) else 0
  total = contribution + numIncreases (y:zs)
numIncreases _ = 0

num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases (u:w:x:y:zs) = total where
  contribution = if (w + x + y) > (u + w + x) then (1 :: Int) else 0
  total = contribution + num3MeasurementIncreases (w:x:y:zs)
num3MeasurementIncreases _ = 0
```

{{% cite HiddingAoC2021-01 %}} basically does:

```hs
diff :: [Int] -> [Int]
diff (a1:a2:as) = a2 - a1 : diff (a2:as)
diff _ = []

numIncreases :: [Int] -> Int
numIncreases = length . filter (> 0) . diff

slidingSum :: [Int] -> [Int]
slidingSum (a1:a2:a3:as) = a1 + a2 + a3 : slidingSum (a2:a3:as)
slidingSum _ = []

num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases = numIncreases . slidingSum
```

Comparing with {{% cite HiddingAoC2021-01 %}}'s solution, mine has hints
of imperative programming. They are working with whole lists, while I'm
more concerned about data shuffling.

{{% cite LeAoC2021-01 %}} notes that combining `drop` and `zipWith`
gives us a way of working with consecutive values:

```hs
numIncreases :: [Int] -> Int
numIncreases xs = length (filter (== True) (zipWith (<) xs (drop 1 xs)))
```

{{% comment %}}

I still have a lot of ground to cover before I shift how I think about
programs. {{% cite HiddingAoC2021-01 %}}'s approach was not on my radar.
Maybe if I wasn't pre-occupied with `[String] -> [Int]` parsing, I'd
have considered something that was more functional-oriented.

{{% /comment %}}

{{% cite HiddingAoC2021-01 %}} goes further and notes that for
`num3MeasurementIncreases`, the middle terms in the finite difference
drop out, and therefore:

```hs
diff3 :: [Int] -> [Int]
diff3 (a1:a2:a3:a4:as) = a4 - a1 : diff3 (a2:a3:as)
diff3 _ = []

num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases = length . filter (> 0) . diff3
```

{{% comment %}}

My `num3MeasurementIncreases` has `if (w + x + y) > (u + w + x)`, and it
didn't occur to me that I can omit `w + x` from both sides of the
inequality check. Huh!

{{% /comment %}}

{{% cite LeAoC2021-01 %}} uses the same idea, comparing `[(a1, a4), (a2,
a5), ...]`:

```hs
num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases xs = length (filter (== True) (zipWith (<) xs (drop 3 xs)))
```

`zipWith` is pretty handy!

I also like the `(x1:x2:x3:xs)` convention over `(w:x:y:zs)`. The former
is more readable.

## References

1. {{< citation
  id="HiddingAoC2021-01"
  author="Johan Hidding"
  title="Advent of Code 2021: Day 1: Sonar Sweep"
  url="https://jhidding.github.io/aoc2021/#day-1-sonar-sweep"
  accessed="2022-02-20" >}}

1. {{< citation
  id="LeAoC2021-01"
  author="Justin Le"
  title="advent-of-code-2021/reflections.md at master · mstksg/advent-of-code-2021"
  url="https://github.com/mstksg/advent-of-code-2021/blob/master/reflections.md#day-1"
  accessed="2022-02-22" >}}

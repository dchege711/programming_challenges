---
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

The first order of business is to figure out how quickly the depth
increases, just so you know what you're dealing with - you never know if
the keys will get carried into deeper water by an ocean current or a
fish or something.

To do this, count **the number of times a depth measurement increases**
from the previous measurement. (There is no measurement before the
first measurement.)

**How many measurements are larger than the previous measurement?**

### Part Two

Considering every single measurement isn't as useful as you expected:
there's just too much noise in the data.

{{% comment %}}

I think going forward, it'll be useful for me to guess what part two of
the problem will be, and see how my guess holds up.

In this case, taking rolling statistics is a technique for smoothening
out noise.

{{% /comment %}}

Instead, consider sums of a **three-measurement sliding window**.

Your goal now is to count **the number of times the sum of measurements
in this sliding window increases** from the previous sum. Stop when
there aren't enough measurements left to create a new three-measurement
sum.

Consider sums of a three-measurement sliding window. **How many sums are
larger than the previous sum?**

## My Solution

{{< readfile
  file=`content/computer-science/programming-challenges/advent-of-code/2021/src/SonarSweep/SonarSweep.lhs`
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
  title="Advent of Code 2021: Day 1: Sonar Sweep"
  url="https://jhidding.github.io/aoc2021/#day-1-sonar-sweep"
  accessed="2022-02-20" >}}

2. {{< citation
  id="LeAoC2021-01"
  title="advent-of-code-2021/reflections.md at master · mstksg/advent-of-code-2021"
  url="https://github.com/mstksg/advent-of-code-2021/blob/master/reflections.md#day-1"
  accessed="2022-02-22" >}}

---
date: 2022-02-23
domains:
- adventofcode.com
- codereview.stackexchange.com
- hackage.haskell.org
- jhidding.github.io
- stackoverflow.com
- wiki.haskell.org
- www.reddit.com
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

## Learning from Others' Solutions

### Potpourri

Although I didn't find a way to declare a type for a `[Int]` whose size
if fixed after reading an input file, it would have still been more
readable to have something like `type Bits = [Int]`, as shown in {{%
cite HiddingAoC2021-03 %}}'s code.

### Computing Most Common and Least Common Bits

My `majorityBits` and `minorityBits` (should have been more aptly named
`mostCommonBits` and `leastCommonBits`) have a lot going in them.

{{% cite HiddingAoC2021-03 %}} has a more elegant implementation which
shows a deeper understanding of math:

```hs
-- The type of `(1 -)` is `Num a => a -> a`. Huh! I can rationalize it
-- as applying eta reduction to `f x = 1 - x`.

invertBinary :: Bits -> Bits
invertBinary = map (1 -)

-- `foldl1 (zipWith (+)) bits` sums up all the bits at a given position.
--
-- If the sum at position `i` is greater than N/2, then we can infer
-- that 1 is more common than 0 at position `i`, and vice-versa if less
-- than N/2. There is a tie if the sum is exactly N/2.
--
-- `(`div` N) . (* 2)` multiplies a number by 2 and then does integer
-- division by N. That is a nifty way of avoiding the N/2 which may not
-- be an integer.

mostCommonBits :: [Bits] -> Bits
mostCommonBits bits = map ((`div` length bits) . (* 2))
                    $ foldl1 (zipWith (+)) bits

leastCommonBits :: [Bits] -> Bits
leastCommonBits = invertBinary . mostCommon
```

... The functions used by {{% cite HiddingAoC2021-03 %}} did not occur
to me at all. Upon reading the problem description, I had an idea of
how I'd go about it, and the only unknowns were translating my plan into
valid Haskell. I should spend more time thinking through my approach
before I rush into coding it.

Several folks in {{% cite rHaskellAoC2021Day03 %}} use {{% cite
Data.List.transpose %}} when calculating `mostCommonBits`. This is
quite convenient as the data for the i-th bit position is all in the
same array!

### Structure of the Solution to Part II

The recursive nature gave me problems. I wonder how others did it. My
approach was:

```hs
lastNumStanding :: [Int] -> Int -> BitFrequencySummarizer -> Int -> Int
lastNumStanding [] _ _ _ = 0
lastNumStanding [x] _ _ _ = x
lastNumStanding nums width f positionToCheck =
  let expectedValues = f width nums
      shouldBeSet = (expectedValues !! positionToCheck) == 1
      bitIndex = length expectedValues - positionToCheck - 1
      matchingNums = filter (\n -> testBit n bitIndex == shouldBeSet) nums
   in if null matchingNums
        then last nums
        else lastNumStanding matchingNums width f (positionToCheck + 1)
```

... and I'm not proud about it. For instance, I keep passing a `width`
parameter that never changes. This parameter was necessitated by the
fact that I represent the bits as an `Int`, and use `testBit` to extract
information. A better abstraction would have been to represent `10111`
as `[1, 0, 1, 1, 1]`, instead of as `23`.

{{% comment %}}

Naming-wise, {{% cite HiddingAoC2021-03 %}}'s `findRating` is better
than `lastNumStanding`. Even better is `rating` as `get*` and `find*`
are not idiomatic.

{{% /comment %}}

{{% cite HiddingAoC2021-03 %}} had a solution of the form:

```hs
findRating :: BitFrequencySummarizer -> Int -> [Bits] -> Bits
-- We don't need to match `findRating _ _ []` because we always have
-- a non-empty `[Bits]` and our base case is the single item `[Bits]`.
findRating _ _ [b] = b
findRating f idx bits =
  -- I don't think that unconditionally calling `findRating` with
  -- the filtered list is safe. What if the filtered list is empty? We'd
  -- go into an infinite loop!
  findRating f (idx + 1)
  -- `(!!)` crashes if the index is out of bounds. However, if we have a
  -- `Data.Vector`, then `(!?)` is safe, and returns `Maybe Int` [2].
  -- Another point in the bag for `Data.Vector`.
  --
  -- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-List.html#v:-33--33-
  -- [2]: https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector.html#v:-33--63-
  $ filter (\b -> b !? idx == mc !? idx) bits
  where mc = f bits
```

### Efficiency of Collections Types

To represent the bit sequence, {{% cite HiddingAoC2021-03 %}} decided to
use `Vector Int` from the [vector
package](https://hackage.haskell.org/package/vector), while I used
`[Int]`.

{{% cite SOPhillipListsArraysVectorSequences %}} discusses tradeoffs:

* Lists give \\(O(1)\\) `cons` and pattern matching, and are purely
  functional and lazy. However, indexing takes \\(O(k)\\) time, they
  have poor data locality, and poor space efficiency (b/c of pointers).

* `Data.Sequence` are purely functional, have amortized \\(O(1)\\) to
  access the beginning and end, and have \\(O(log\ n)\\) access to the
  middle. However, poor data locality, and only work for finite
  collections.

* Arrays provide \\(O(1)\\) access, and have good data locality, but they
  do not fit very well with the lazy pure functional world, and are a
  pain to use.

* `Data.Vector` provides all of the array goodness in a higher level and
  cleaner APIs, with caveats like mutable arrays don't place nice with
  pure lazy languages.

{{% comment %}}

{{% cite SOPhillipListsArraysVectorSequences %}} mentions `[Char]` as a
notable example of lists screwing up performance, and therefore a
recommendation to use `Data.Text` or the very fast `Data.ByteString`.
{{% cite HWikiPerfStrings %}} has sample perf numbers.

{{% /comment %}}

### Converting Binary Representation to Decimal

My solution was:

```hs
fromBitList :: [Int] -> Int
fromBitList ds = fst $ foldr f (0, 1) ds
  where
    f d (s, powerOf2) = (s + powerOf2 * d, powerOf2 * 2)
```

... I don't like that I'm carrying over two pieces of information in the
fold.

{{% cite HiddingAoC2021-03 %}} had a solution of the form:

```hs
binaryToDecimal :: [Int] -> Int
binaryToDecimal = go 0
  where go n (b:bs) = go (2*n + b) bs
        go n []     = n

-- The recursive `go` is tricky. Tracing a sample call (with fully
-- evaluated parameters for clarity)
--
--                                = go  0 [1, 0, 1, 1, 1]
--  = go ( 2*0 + 1)  [0, 1, 1, 1] = go  1    [0, 1, 1, 1]
--  = go ( 2*1 + 0)     [1, 1, 1] = go  2       [1, 1, 1]
--  = go ( 2*2 + 1)        [1, 1] = go  5          [1, 1]
--  = go ( 2*5 + 1)           [1] = go 11             [1]
--  = go (2*11 + 1)           [0] = go 23              []
--  = 23
```

Even after tracing through {{% cite HiddingAoC2021-03 %}}'s recursive
`fromBinary`, I'm not sure how one could come up with it. Building up
the solution from the least significant bit is more intuitive to me, not
so for the most-significant-bit-first approach. Maybe algebraic
manipulation might help?

$$ 10111_2 = (2^4 \cdot 1) + (2^3 \cdot 0) + (2^2 \cdot 1) + (2^1 \cdot 1) + (2^0 \cdot 1) $$

The algebraic form didn't help. Maybe the intuition for the MSB-first
approach is that whenever we see a new digit `b`, we need to multiply
whatever we have by `2` and then add `b`. That is the best answer we can
give _at that moment_. The various values for `n` in the `go n` calls
are \\(0_2, 1_2, 10_2, 101_2, 1011_2, 10111_2\\).

{{% cite SESam2021 %}} uses a left-fold:

```hs
binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + x) 0
```

... which is basically what {{% cite HiddingAoC2021-03 %}} did, but
using the standard library. That said, if using a left fold, the strict
`foldl'` is a better choice to avoid stack overflow of pending thunks.
{{% cite Data.List.foldl %}}

## References

1. {{< citation
  id="HiddingAoC2021-03"
  title="Advent of Code 2021: Day 3: Binary Diagnostic"
  url="https://jhidding.github.io/aoc2021/#day-3-binary-diagnostic"
  accessed="2022-02-25" >}}

1. {{< citation
  id="SOPhillipListsArraysVectorSequences"
  title="Haskell: Lists, Arrays, Vectors, Sequences - Stack Overflow"
  url="https://stackoverflow.com/a/9613203/7812406"
  accessed="2022-02-25" >}}

1. {{< citation
  id="HWikiPerfStrings"
  title="Performance/Strings - HaskellWiki"
  url="https://wiki.haskell.org/Performance/Strings"
  accessed="2022-02-25" >}}

1. {{< citation
  id="SESam2021"
  title="performance - Advent of Code 2021, Day 3 in Haskell - Code Review Stack Exchange"
  url="https://codereview.stackexchange.com/questions/270654/advent-of-code-2021-day-3-in-haskell"
  accessed="2022-02-25" >}}

1. {{< citation
  id="Data.List.foldl"
  title="Data.List > foldl'"
  url="https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html#v:foldl-39-" accessed="2022-02-25" >}}

1. {{< citation
  id="rHaskellAoC2021Day03"
  title="Advent of Code 2021 day 3 : haskell"
  url="https://www.reddit.com/r/haskell/comments/r7r1g8/advent_of_code_2021_day_3/"
  accessed="2022-02-25" >}}

1. {{< citation
  id="Data.List.transpose"
  title="Data.List.transpose"
  url="https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html#v:transpose"
  accessed="2022-02-25" >}}

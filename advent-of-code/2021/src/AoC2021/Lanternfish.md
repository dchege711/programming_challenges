---
title: "AoC 2021 Day 06: Lanternfish"
date: 2022-03-01
weight: 6
---

## Part I Description

*The sea floor is getting steeper. Maybe the sleigh keys got carried this way?*

*A massive school of glowing lanternfish swims past. They must spawn quickly to
reach such large numbers - maybe exponentially quickly? You should model their
growth to be sure.*

*Although you know nothing about this specific species of lanternfish, you make
some guesses about their attributes. Surely, each lanternfish creates a new
lanternfish once every 7 days.*

*However, this process isn't necessarily synchronized between every lanternfish -
one lanternfish might have 2 days left until it creates another lanternfish,
while another might have 4. So, you can model each fish as a single number that
represents the number of days until it creates a new lanternfish.*

*Furthermore, you reason, a new lanternfish would surely need slightly longer
before it's capable of producing more lanternfish: two more days for its first
cycle.*

*A lanternfish that creates a new fish resets its timer to `6` not `7` (because
`0` is included as a valid timer value). The new lanternfish starts with an
internal time of `8` and does not start counting down until the next day.*

*Realizing what you're trying to do, the submarine automatically produces a list
of the ages of several hundred lanternfish (your puzzle input).*

*Find a way to simulate lanternfish. **How many lanternfish would there be after
80 days?***

## Part I Solution

The input is of the form:

```txt
3,4,3,1,2
```

... so nothing special with parsing. Using `[Int]` should do just fine.

```hs
{-#  LANGUAGE RecordWildCards  #-}
{-#  OPTIONS_GHC -Wall  #-}

module AoC2021.Lanternfish (numOfFishIn80Days, numOfFishIn256Days) where

import Data.List (group, sort, foldl')
```

~~If the lanternfish's internal timers were all in sync, then their population
on day \\(d\\) is of the form \\(\lfloor n_0 \cdot r \cdot d \rfloor \\), where
\\(r\\) factor of growth per day.~~ Was unable to get a value for \\(r\\).

{{% comment %}}

There should be some established literature on population growth modeling. For
example, ORF 309 covered the Galton-Watson process, which is basically this
question, with the descendants being born with some probability, and everyone in
sync.

I wonder how close I'll get from reasoning my way through.

{{% /comment %}}

The case where \\(n_0 = 1\\), and the internal timer for the sole lanternfish
starts with a value of `0` should help determine \\(r\\). At the end of
\\(d=1\\), we'd have two lanternfish \\([l_1, l_2]\\) with internal timers of
`6` and `8` respectively. At the end of \\(d = 8\\), \\(l_1\\) would produce
\\(l_3\\), and at the end of \\(d = 10\\), \\(l_2\\) will produce \\(l_4\\).
Enumerating the days when \\(l_i\\) produces a new \\(l_j\\):

$$ l_1 (d_{birth} = ?) \to 1, 8, 15, ... $$
$$ l_2 (d_{birth} = 1) \to 10, 17, 24, ... $$
$$ l_3 (d_{birth} = 8) \to 17, 24, 31, ... $$
$$ l_4 (d_{birth} = 10) \to 19, 26, 33, ... $$

Each case is an arithmetic series whose first term differs, but the difference
between consecutive terms is always `7`. The number of fish at \\(d\\) is the
number of terms less than \\(d\\) in the expressions above, plus \\(n_0\\).

The sample input has starting internal timers of `3, 4, 3, 1, 2`. Using the
notation above, and enumerating the individuals born at \\(d \le 7\\):

$$ l_4 (d_{birth} = ?) \to 2, 9, 16, ... $$
$$ l_5 (d_{birth} = ?) \to 3, 10, 17, ... $$
$$ l_1 (d_{birth} = ?) \to 4, 11, 18, ... $$
$$ l_3 (d_{birth} = ?) \to 4, 11, 18, ... $$
$$ l_2 (d_{birth} = ?) \to 5, 12, ... $$
$$ l_6 (d_{birth} = 2) \to 11, 18, ... $$
$$ l_7 (d_{birth} = 3) \to 12, 19, ... $$
$$ l_8 (d_{birth} = 4) \to 13, 20, ... $$
$$ l_9 (d_{birth} = 4) \to 13, 20, ... $$
$$ l_{10} (d_{birth} = 5) \to 14, 21, ... $$

... the sum of individuals is \\(5 + 5 = 10\\). I'm not sure how to turn this
into an algorithm that doesn't enumerate all individuals at day \\(d\\), and
then computes the individuals at \\(d+1\\), and so forth until we get to
\\(d=80\\).

Given an individual \\(l_i\\), we can compute the number of direct children the
individual will have by \\(d = 80\\). However, we need to do this recursively
as the children will also have children.

```hs
_numOfDirectChildren :: [Int] -> Int -> Int
_numOfDirectChildren (deliveryDay : deliveryDays) finalDay =
    let
        childrenBirthdays = [deliveryDay, deliveryDay + 7 .. finalDay]
        childrenDeliveryDays = map (+9) childrenBirthdays
    in length childrenBirthdays + _numOfDirectChildren
        (deliveryDays ++ childrenDeliveryDays) finalDay
_numOfDirectChildren [] _ = 0

_numOfFishIn80Days' :: [Int] -> Int
_numOfFishIn80Days' internalTimers =
    let deliveryDays = map (+1) internalTimers
    in length internalTimers + _numOfDirectChildren deliveryDays 80
```

Debugging a recursive function using breakpoints is tedious. {{% cite
HaskellWikiDebugging %}} mentions the `Hood` package which can print all
invocations of a function `f` and their results. However, `cabal install hood`
fails because `FPretty` (dependency of `Hood`) requires `base>=4.5 && <4.11`,
while I have `base==4.14.3.0` installed. I don't want to demote by `base`.

{{% open-comment %}}

Debug `_numOfFishIn80Days'`. I don't know why it hangs indefinitely.

{{% /open-comment %}}

Let's try a more straightforward (but less efficient?) approach, where we track
individuals in `[Int]`.

```hs
simulateFishGrowth :: [Int] -> [Int] -> [Int]
simulateFishGrowth (_:days) internalTimers =
    let nextUnadjustedTimers = map (\t -> t - 1) internalTimers
        numChildren = length $ filter (< 0) nextUnadjustedTimers
        adjustedTimers = map (\v -> if v < 0 then 6 else v) nextUnadjustedTimers
    in simulateFishGrowth days (adjustedTimers ++ replicate numChildren 8)

simulateFishGrowth [] internalTimers = internalTimers

numOfFishIn80Days :: [Int] -> Int
numOfFishIn80Days = length . simulateFishGrowth [1 .. 80]
```

## Part II Description

{{% comment %}}

Expected extension for Part II: accounting for the fact that lanternfish die
after some time. A lanternfish does not keep reproducing indefinitely.

Update: Wrong guess. Death wouldn't complicate `simulateFishGrowth` too much.
the major change would be instead of tracking only the internal timers, we'd
track the age of the lanternfish. Immutable lists would lead to a lot of
copying.

{{% /comment %}}

*Suppose the lanternfish live forever and have unlimited food and space. Would
they take over the entire ocean?*

*After 256 days in the example above, there would be a total of 26,984,457,539
lanternfish!*

***How many lanternfish would there be after 256 days?***

## Part II Solution

Simulating 256 days will cripple `simulateFishGrowth`. In the sample input,
we'll have a list with 27B items, up from 6k items! If the the same explosion is
applied to the puzzle input, we're expecting a final list with
\\(360{,}610 \cdot (26{,}984{,}457{,}539 / 5{,}934) \approx
1{,}639{,}849{,}213{,}538 \\) items. Under the rosiest projections, each entry
in the `[Int]` takes 8 bytes, we need
\\(\ge 1{,}639{,}849{,}213{,}538 \cdot 8 / 1{,}024^4 \approx 11.93 \text{TB}\\).
That's beyond my laptop's range.

```hs
--  Takes too long
_numOfFishIn256Days' :: [Int] -> Int
_numOfFishIn256Days' = length . simulateFishGrowth [1 .. 256]
```

One more attempt before looking up how to model unbounded population growth.
There exists some cohort of lanternfish that are giving birth on the same day.
If we have a lanternfish that reproduces on \\(d = [0, 7, 14, ...]\\), the
second generation will reproduce on \\(d = [9, 16, ...]\\), the third on \\(d =
[16, 23, ...]\\), the fourth on \\(d = [23, 30, 37, ...]\\), and so forth.
Hmm... I don't see it. Time to consult population growth literature.

The term for the kind of growth exhibited by the lanternfish is exponential
growth, because each lanternfish gives forth one lanternfish. Exponential growth
is described by \\(x_t = x_0 \cdot b^{t/\tau}\\), where \\(b\\) is a positive
growth factor, and \\(\tau\\) is the time required for \\(x\\) to grow by one
factor of \\(b\\). {{% cite WikiExpGrowth %}} The simple case where each
lanternfish creates a new one every seven days and is in sync with every other
lanternfish, can be modelled by \\(x_t = x_0 \cdot 2^{t/7}\\). I still don't see
a way of adapting \\(x_t = x_0 \cdot b^{t/\tau}\\) to account for the out of
phase internal timers, and the initial \\(\tau = 7\\).

What about maintaining a list of `InternalTimer {t :: Int, count :: Int}`. This
list will have at most 9 items because `t` can only assume values in
\\([0,1,...9]\\). Then on each day, we can update this list. The memory blow-up
experienced earlier is avoided by collapsing all of the `n` items of value `t`
in the original `[Int]` into a single `InternalTimer{t = t, count = n}`.

{{% comment %}}

Now that I type this out, why didn't it hit me sooner?

{{% /comment %}}

```hs
data InternalTimers = InternalTimers {
    t0 :: Integer, t1 :: Integer, t2 :: Integer, t3 :: Integer, t4 :: Integer,
    t5 :: Integer, t6 :: Integer, t7 :: Integer, t8 :: Integer} deriving Show

updateInternalTimers :: InternalTimers -> Int -> InternalTimers
updateInternalTimers InternalTimers{ .. } _ =
    let originalT0 = t0
    in InternalTimers{t0 = t1, t1 = t2, t2 = t3, t3 = t4, t4 = t5, t5 = t6,
                      t6 = t7 + originalT0, t7 = t8, t8 = originalT0}
```

Ah, too much data shifting. A circular list fits better, so that we're only
moving the pointer, and not the data itself. But given that Haskell values are
immutable, we don't have much to gain (and potentially more to lose as we add
an the complexity of implementing circular list and locating a given cohort)?

```hs
numPossibleInternalTimers :: Int
numPossibleInternalTimers = 9 --  [0, 1, 2, ..., 8]

extractCohortCounts :: [[Int]] -> [Integer]
extractCohortCounts (l:ls) =
    let currentCohort = head l
        numFillers = if null ls && currentCohort > 0
            then currentCohort
            else currentCohort - head (head ls) - 1
        cohortCountEntries = toInteger (length l) : replicate numFillers (0 :: Integer)
    in cohortCountEntries ++ extractCohortCounts ls
extractCohortCounts [] = []

internalTimersFromRawData :: [Int] -> InternalTimers
internalTimersFromRawData ts =
    let
        reversedGrouping = group $ reverse $ sort ts
        possiblyPartialCohorts = extractCohortCounts reversedGrouping

        frontPaddingSize = if null reversedGrouping
            then numPossibleInternalTimers
            else numPossibleInternalTimers - head (head reversedGrouping) - 1

        extractedCohortCounts = reverse possiblyPartialCohorts ++
                                replicate frontPaddingSize 0

        internalTimers = InternalTimers{..} where
            [t0, t1, t2, t3, t4, t5, t6, t7, t8] = extractedCohortCounts
    in internalTimers

numOfFishIn256Days :: [Int] -> Integer
numOfFishIn256Days rawTimers =
    let
        days = [1, 2 .. 256]
        internalTimers = internalTimersFromRawData rawTimers
        finalTimers = foldl' updateInternalTimers internalTimers days
        totalPopulation = t0 finalTimers + t1 finalTimers + t2 finalTimers
                        + t3 finalTimers + t4 finalTimers + t5 finalTimers
                        + t6 finalTimers + t7 finalTimers + t8 finalTimers
    in totalPopulation
```

## References

1. {{< citation
    id="HaskellWikiDebugging"
    title="Debugging - HaskellWiki"
    url="https://wiki.haskell.org/Debugging"
    accessed="2022-03-02" >}}

1. {{< citation
  id="WikiExpGrowth"
  title="Exponential Growth - Wikipedia"
  url="https://en.wikipedia.org/wiki/Exponential_growth"
  accessed="2022-02-22" >}}

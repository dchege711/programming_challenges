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
module AoC2021.Lanternfish (numOfFishIn80Days) where
```

If the lanternfish's internal timers were all in sync, then their population on
day \\(d\\) is of the form \\(\lfloor n_0 \cdot r \cdot d \rfloor \\), where
\\(r\\) factor of growth per day.

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
_numOfFishIn256Days' :: [Int] -> Int
_numOfFishIn256Days' = length . simulateFishGrowth [1 .. 256]
```

## References

1. {{< citation
    id="HaskellWikiDebugging"
    title="Debugging - HaskellWiki"
    url="https://wiki.haskell.org/Debugging"
    accessed="2022-03-02" >}}

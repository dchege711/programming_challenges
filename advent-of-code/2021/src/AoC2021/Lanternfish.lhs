%include polycode.fmt
---
title: "AoC 2021 Day 06: Lanternfish"
date: 2022-03-01
weight: 6
---

\## Part One

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

\begin{code}
module AoC2021.Lanternfish (numOfFishIn80Days) where

numOfFishIn80Days :: [Int] -> Int
numOfFishIn80Days _ = 5

\end{code}

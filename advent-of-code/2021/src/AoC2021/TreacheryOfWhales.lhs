%include polycode.fmt
---
title: "AoC 2021 Day 07: The Treachery of Whales"
date: 2022-03-05
weight: 7
---

\## Part I Description

*A giant whale has decided that your submarine is its next meal, and it's much
faster than you are. There's nowhere to run!*

*Suddenly, a swarm of crabs (each in its own tiny submarine - it's too deep for
them otherwise) zooms in to rescure you! They seem to be preparing to blast a
hole in the ocean floor; sensors indicate a massive underground cave system just
beyond where they're aiming!*

*The crab submarines all need to be aligned before they'll have enough power to
blast a large enough hole for your submarine to get through. However, it doesn't
look like they'll be aligned before the whale catches you! Maybe you can help?*

*There's one major catch - crab submarines can only move horizontally.*

*You quickly make a list of the horizontal position of each crab (your puzzle
input). Crab submarines have limited fuel, so you need to find a way to make
all of their horizonatal positions match, while requiring them to spend as
little fuel as possible.*

*Determine the horizontal position that the crabs can align to using the least
fuel possible. **How much fuel must they spend to align to that position?***

\## Part I Solution

The challenge is finding the number that minimizes the sum of differences in the
whole list. My gut is to test the mean, mode, and median, with emphasis on the
mean (as the definition of standard deviation uses the mean).

The sample input is \\([0,1,1,2,2,2,4,7,14,16]\\), and the number that minimizes
the sum of differences is \\(2\\). The three statistics are: mean = \\(4.9\\),
mode = \\(2\\), and median = \\(2\\). My hunch on using the mean was wrong, it
would not have gotten me to \\(2\\).

Is the mode the best candidate? In \\([0,0,7,8,9]\\), the mode is \\(0\\), and
choosing it leads to \\(0 + 0 + 7 + 8 + 9 = 24\\), while choosing \\(8\\) would
give us \\(8 + 8 + 1 + 0 + 1 = 18\\). So the mode does not always give the
correct answer.

Is the median the best candidate? In \\([0,0,0,7,9,9,15]\\), the median is
\\(7\\), and choosing it leads to \\(7 + 7 + 7 + 0 + 2 + 2 + 8 = 33\\). Choosing
\\(9\\) leads to \\(9 + 9 + 9 + 2 + 0 + 0 + 6 = 35\\). Choosing \\(8\\) gives
\\(8 + 8 + 8 + 1 + 1 + 1 + 7 = 34\\). Hmm, maybe this is it? With
\\([0,0,0,0,10,10,10]\\), the median (\\(0\\)) gives \\(30\\). Choosing \\(6\\)
gives \\(6 \cdot 4 + 4 \cdot 3 = 36\\), \\(5\\) gives \\(5 \cdot 4 + 4 \cdot 3
= 32\\), and \\(4\\) gives \\(4 \cdot 4 + 6 \cdot 3 = 34\\). A counterexample is
proving elusive. Maybe the median works.

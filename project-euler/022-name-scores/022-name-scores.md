---
date: 2022-02-18
domains:
- projecteuler.net
local_url: http://localhost:1313/computer-science/programming-challenges/project-euler/022-name-scores/022-name-scores/
title: 022. Name Scores
---

{{< citation
  id="ProjectEuler022"
  title="#22 Names scores - Project Euler"
  url="https://projecteuler.net/problem=22"
  accessed="2022-02-18">}}

## Problem Statement

Using `names.txt`, a 46K text file containing over 5,000 first names,
begin by sorting it into alphabetical order. Then working out the
alphabetical value for each name, multiply this value by its
alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN,
which is worth \\(3 + 15 + 12 + 9 + 14 = 53\\), is the 938th name in the
list. So COLIN would obtain a score of \\(938 \times 53 = 49{,}714\\).

What is the total of all name scores in the file?

## My Solution

This problem is a brute-force kind of problem, so no fancy shortcuts can
make it feasible to solve by hand. Will solve this in Haskell to
continue learning more about functional programming.

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/022-name-scores/name_scores.hs`
  highlight="haskell" >}}

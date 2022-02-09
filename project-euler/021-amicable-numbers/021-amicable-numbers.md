---
title: 021. Amicable Numbers
date: 2021-02-06
draft: true
weight: 21
tags:
- programming-environment
- haskell
---

## Problem Statement {{% cite ProjectEuler021 %}}

Let \\(d(n)\\) be defined as the sum of proper divisors of \\(n\\) (numbers less
than \\(n\\) which divide evenly into \\(n\\)).

If \\(d(a) = b\\) and \\(d(b) = a\\), where \\(a \\neq b\\), then \\(a\\) and
\\(b\\) are an amicable pair and each of \\(a\\) and \\(b\\) are called amicable
numbers.

For example, the proper divisors of \\(220\\) are \\(1, 2, 4, 5, 10, 11, 20, 22,
44, 55, 110\\); therefore \\(d(220) = 284\\). The proper divisors of \\(284\\)
are \\(1, 2, 4, 71, 142\\); so \\(d(284) = 220\\).

Evaluate the sum of all the amicable numbers under \\(10{,}000\\).

## My Solution

I don't think this problem can be efficiently solved manually. Too much grunt
work.

For this problem, I'll try out Haskell. {{% cite HaskellDocs %}} lists several
resources and recommends UPenn's {{% cite CIS194Spring2013 %}} for getting
started. {{% cite SchoolOfHaskell %}} is by the same author, and seems more
applicable and less academic-focused.

{{% comment %}}

{{% cite CIS194Spring2013 %}} provides lecture slides as .html and .lhs files.
Investigating .lhs files introduced me to literate programming, whose main idea
is to regard a program as a communication to human beings rather than as a set
of instructions to a computer. Nifty! There is even multi-mode support in Emacs,
which switches between haskell-mode and latex-mode depending on where the cursor
is. {{% cite HaskellWikiLiterateProgramming %}}

Pays homage to nothing new under the sun. I thought Jupyter notebooks were quite
fancy, but Literate Haskell already existed. Granted, Jupyter has bells and
whistles for interaction.

{{% /comment %}}

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/021-amicable-numbers/amicable_numbers.lhs`
  highlight="haskell" >}}

The 45s running time is pretty bad considering that this Python script takes 6s:

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/021-amicable-numbers/amicable_numbers.py`
  highlight="python" >}}

## References

1. {{< citation
  id="ProjectEuler021"
  title="#21 Amicable numbers - Project Euler"
  url="https://projecteuler.net/problem=21"
  accessed="2022-02-06">}}

1. {{< citation
  id="HaskellDocs"
  title="Documentation"
  url="https://www.haskell.org/documentation/"
  accessed="2022-02-06">}}

1. {{< citation
  id="HaskellWikiLiterateProgramming"
  title="Literate programming - HaskellWiki"
  url="https://wiki.haskell.org/Literate_programming"
  accessed="2022-02-06">}}

1. {{< citation
  id="CIS194Spring2013"
  author="Brent Yorgey"
  title="Lecture notes and assignments"
  url="https://www.seas.upenn.edu/~cis194/spring13/lectures.html"
  accessed="2022-02-06">}}

1. {{< citation
  id="SchoolOfHaskell"
  author="Brent Yorgey"
  title="Starting with Haskell - School of Haskell | School of Haskell"
  url="https://www.schoolofhaskell.com/user/school/starting-with-haskell"
  accessed="2022-02-08">}}

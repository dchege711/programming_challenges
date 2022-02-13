---
title: 021. Amicable Numbers
date: 2021-02-06
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

I don't think this problem can be efficiently solved manually. ~~Too much grunt
work.~~ Given a number's prime factorization, {{% cite PlanetMathSumOfDivisors
%}} provides a formula for summing up the divisors.

For this problem, I'll try out Haskell. {{% cite HaskellDocs %}} lists several
resources and recommends UPenn's {{% cite CIS194Spring2013 %}} for getting
started. {{% cite SchoolOfHaskell %}} is by the same author, and seems more
applicable and less academic-focused. {{% cite Hoogle %}} is a Haskell API
search engine that helps one avoid re-implementing functionality from the
Haskell library. There are VS Code extensions that use {{% cite HLint %}} to
offer contextual suggestions; they should help me hone my Haskell.

{{% comment %}}

{{% cite CIS194Spring2013 %}} provides lecture slides as .html and .lhs files.
Investigating .lhs files introduced me to literate programming, whose main idea
is to regard a program as a communication to human beings rather than as a set
of instructions to a computer. Nifty! There is even multi-mode support in Emacs,
which switches between haskell-mode and latex-mode depending on where the cursor
is. {{% cite HaskellWikiLiterateProgramming %}}

Pays homage to "nothing new under the sun". I thought Jupyter notebooks were
quite fancy, but Literate Haskell already existed. Granted, Jupyter has bells
and whistles for interactive programming.

{{% /comment %}}

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/021-amicable-numbers/amicable_numbers.lhs`
  highlight="haskell"
  id="PE021Haskell" >}}

The 45s running time is pretty bad considering that a similar Python script
takes 6s.

{{% comment %}}

```py
def sum_of_divisors(n):
    s = 0
    for i in range(1, n):
        if n % i == 0: s += i
    return s

def sum_of_divisors_slow(n):
    def is_factor(f): return n % f == 0
    return sum(filter(is_factor, range(1, n)))
```

I'm surprised that using `sum_of_divisors_slow` takes about twice the time as
using `sum_of_divisors`. I expected `sum_of_divisors` to be faster (given how
for-loops go brrr), but not by much.

{{% /comment %}}

## Learning From Others' Solutions

### Minimizing the Number of Calls to the `SumOfProperDivisors` Function

A construction like:

```py
for a in range(1, 10000): # 1, 2, 3, ..., 9999
  for b in range(a + 1, 10000): # a + 1, a + 2, ..., 9999
    if sum_of_proper_divisors(a) == b and sum_of_proper_divisors(b) == a: # ...
```

... calls `sum_of_proper_divisors` at most \\((9{,}999 \times 9{,}998)\\) times.

{{% comment %}}

Computing \\((9{,}998 \times 9{,}997) / 2\\): The inner for-loop executes
\\(9{,}998 + 9{,}997 + ... + 1\\) times. The closed form of \\(1 + 2 + 3 + ... +
n\\) is \\(n \cdot (n + 1) / 2\\). {{% cite BrilliantSumOfN %}}

{{% /comment %}}

However, if we calculate `b = sum_of_proper_divisors(a)`, then
`sum_of_proper_divisors(b)` should be equal to `a`, and therefore we can have:

```py
s = 0
for a in range(1, 10000): # 1, 2, 3, ..., 9999
  b = sum_of_proper_divisors(a) # Given `a`, there's at most one possible `b`
  if b > a: # Avoid double counting (a, b) and (b, a)
    if sum_of_proper_divisors(b) == a: s += a + b
```

... which calls `sum_of_proper_divisors` at most \\((9{,}999 \times 2)\\) times.
{{% cite ProjectEuler021Solutions %}}

{{% comment %}}

My [Haskell solution](#PE021Haskell) calls `sum_of_proper_divisors` at most
\\((9{,}999 \times 3)\\) times. \\(\times 3\\) because I neither cache `b` [1],
nor do I check `(b > a)` [2]. Still, it did not occur to me that looping through
possible pairs \\((a, b)\\) was also an available, albeit inefficient, option.

Applying these two optimizations halves the running time from 45s to 23s.

[1]: I did not cache `b` because I wasn't that familiar with Haskell's syntax
for functions with multiple expressions.

[2]: I did not check `(b > a)` because it wasn't on my radar. My approach
involved deciding if each number in \\([1, 2, ..., 9{,}999]\\) was amicable or
not. For amicable pair \\((a, b)\\), I included \\(a\\) in the sum, and
disregarded \\(b\\) until I got to the pair \\((b, a)\\).

{{% /comment %}}

### Computing `SumOfProperDivisors` Efficiently

To find the proper divisors of \\(n\\), we need not check from \\(1\\) to
\\(n\\). At a first approximation, we only need to check till \\(n/2\\).
However, we can go even further and only check till \\(\sqrt{n}\\), and use the
fact that if \\(i : i \le \lfloor \sqrt{n} \rfloor \\) is a divisor, then
\\(n/i\\) is also a proper divisor of \\(n\\). Furthermore, odd numbers cannot
have even numbers as divisors. {{% cite ProjectEuler021Solutions %}}

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/021-amicable-numbers/amicable_numbers_deluxe.hs`
  highlight="haskell"
  id="PE021HaskellOptimizedSumDivisorsLoop" >}}

{{% comment %}}

Applying this optimization slashes the execution time from 23s to 1s.

{{% /comment %}}

Suppose that \\(n\\) is a positive integer whose factorization into prime
factors is \\(\prod_{i=1}^k p_{i}^{m_i}\\). Notice that any divisor \\(d\\) must
be a product of some number of each of the prime factors, i.e. \\( \prod_{i=1}^k
p_{i}^{\mu_i} \\), where \\(0 \le \mu_i \le m_i\\). Then the sum over all
divisors becomes the sum over all possible choices for the \\(\mu_{i}\\)'s {{%
cite PlanetMathSumOfDivisors %}}

$$ \sum_{ d \mid n} d = \sum_{0 \le \mu_i \le m_i\} \prod_{i=1}^k p_{i}^{\mu_i}\
= \sum_{\mu_1 = 0}^{m_1} \sum_{\mu_2=0}^{m_2} ... \sum_{\mu_k=0}^{m_k} \prod_{i=1}^k p_{i}^{\mu_i}\
= \prod_{i=1}^k \left( \sum_{\mu_i = 0}^{m_i} p_{i}^{\mu_i} \right)\
= \prod_{i=1}^k \frac{p_{i}^{m_i + 1} - 1}{p_i - 1} $$

{{% comment %}}

{{% cite PlanetMathSumOfDivisors %}} uses several mathematical maneuvers:
expressing sum as a multiple sum, factoring a sum of products into a product of
sums, and recognizing that each sum is a geometric series.

{{% /comment %}}

For instance, the factorization of \\(220\\) is \\(2^2 \cdot 5^1 \cdot 11^1\\),
and therefore the sum of its proper divisors is:

$$ \left( \frac{2^3 - 1}{1} \cdot \frac{5^2 - 1}{4} \cdot \frac{11^2 - 1}{10} \right) - 220 = 284 $$

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/021-amicable-numbers/amicable_numbers_deluxe.py`
  highlight="python" >}}

{{% open-comment %}}

Solve PE #021 using the sum of divisors formula in Haskell. The Python
implementation runs in 0.1s (compared to the 6s Python equivalent of [this naïve
approach](#PE021Haskell)).

{{% /open-comment %}}

### Trivia

Amicable numbers have history to them. For example, Genesis 32:14 has Jacob
giving Esau 220 sheep when he was afraid that Esau was going to kill him. {{%
cite CaldwellAmicableNums %}}

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

1. {{< citation
  id="ProjectEuler021Solutions"
  title="Thread & Overview for 21 - Project Euler"
  url="https://projecteuler.net/thread=21"
  url_2="https://projecteuler.net/overview=021"
  accessed="2022-02-09">}}

1. {{< citation
  id="CaldwellAmicableNums"
  title="The Prime Glossary: amicable numbers"
  author="Chris K. Caldwell"
  url="https://primes.utm.edu/glossary/page.php?sort=AmicableNumber"
  accessed="2022-02-09">}}

1. {{< citation
  id="PlanetMathSumOfDivisors"
  title="formula for sum of divisors"
  url="https://planetmath.org/formulaforsumofdivisors"
  accessed="2022-02-12">}}

1. {{< citation
  id="BrilliantSumOfN"
  title="Sum of n, n², or n³ | Brilliant Math & Science Wiki"
  url="https://brilliant.org/wiki/sum-of-n-n2-or-n3/"
  accessed="2022-02-12">}}

1. {{< citation
  id="Hoogle"
  title="Hoogle"
  url="https://hoogle.haskell.org/"
  accessed="2022-02-12">}}

1. {{< citation
  id="HLint"
  title="ndmitchell/hlint: Haskell source code suggestions"
  url="https://github.com/ndmitchell/hlint"
  accessed="2022-02-12">}}

---
date: 2023-02-19
domains:
- docs.python.org
- mathworld.wolfram.com
- projecteuler.net
- wiki.python.org
- www.hackerrank.com
local_url: http://localhost:1313/computer-science/programming-challenges/project-euler/023-non-abundant-sums/023-non-abundant-sums/
title: 023. Non-Abundant Sums
weight: 23
---

{{< citation
  id="ProjectEuler023"
  title="#23 Non-abundant sums - Project Euler"
  url="https://projecteuler.net/problem=23"
  accessed="2023-02-19" >}}

## Problem Statement

A **perfect number** is a number for which the sum of its proper
divisors is exactly equal to the number. For example, the sum of the
proper divisors of \\(28\\) would be \\(1 + 2 + 4 + 7 + 14 = 28\\),
which means that \\(28\\) is a perfect number.

A number \\(n\\) is called **deficient** if the sum of its proper
divisors is less than \\(n\\), and it is called **abundant** if this sum
exceeds \\(n\\).

As \\(12\\) is the smallest abundant number, \\(1 + 2 + 3 + 4 + 6 =
16\\), the smallest number that can be written as the sum of two
abundant numbers is \\(24\\). By mathematical analysis, it can be shown
that all integers greater than \\(28,123\\) can be written as the sum of
two abundant numbers. However, this upper limit cannot be reduced any
further by analysis even though it is known that the greatest number
that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the
sum of two abundant numbers.

## My Solution

{{% comment %}}

To work with .py file(s) in the Python REPL, first `cd` into the
directory containing the .py file(s), and then from the REPL:

```py
>>> from brute_force_non_abundant_sums import *
```

To refresh the contents after editing the file:

```py
>>> import importlib
>>> importlib.reload(brute_force_non_abundant_sums)
>>> from brute_force_non_abundant_sums import *
```

{{% cite PythonImportLib %}}

{{% /comment %}}

With [`sum_of_proper_divisors(n)` from PE 021]({{< ref
"/computer-science/programming-challenges/project-euler/021-amicable-numbers/021-amicable-numbers#computing-sumofproperdivisors-efficiently"
>}}), a brute-force algorithm is:

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/023-non-abundant-sums/non_abundant_sums.py`
  highlight="python"
  id="PE023BruteForcePy" >}}

Performance-wise:

```log
4179871.0
         12183909 function calls in 4.462 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    3.357    3.357    4.460    4.460 brute_force_non_abundant_sums.py:36(pairwise_sums)
 12148815    0.924    0.000    0.924    0.000 {method 'add' of 'set' objects}
    28122    0.172    0.000    0.172    0.000 brute_force_non_abundant_sums.py:8(sum_of_proper_divisors)
     6966    0.006    0.000    0.179    0.000 brute_force_non_abundant_sums.py:29(generate_abundant_nums)
        1    0.002    0.002    4.462    4.462 brute_force_non_abundant_sums.py:49(sum_of_non_abundant_sums)
        1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
        1    0.000    0.000    4.462    4.462 {built-in method builtins.exec}
        1    0.000    0.000    4.462    4.462 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
```

How much better can `pairwise_sums` get? Well, `nums` is \\(O(N)\\), and
\\(\_{N}C_{2} = \frac{N \cdot (N-1)}{2 \cdot 1} = O(N^2)\\). Although
generating the pairwise sums in a monotonically increasing order can
avoid generating \\(n > N\\), the overall runtime will still be
\\(O(N^2)\\). How much additional work are we doing anyway? Logging
shows that \\(\approx 50\\%\\) of the sums generated greater than
\\(N\\), and so a better `pairwise_sums` should shave about half of the
total running time. That's an optimization worth pursuing.

{{% open-comment %}}

Why logging gave a quick and precise answer, is this hand-waving
correct? Assuming that the abundant nums are more or less uniformly
spread out in \\([1, ...,  N]\\), their pairwise sums should also be
uniformly spread out in \\([2, ..., 2N]\\), and therefore \\(\approx
50\\%\\) of the sums will be greater than \\(N\\).

{{% /open-comment %}}

## Generating Monotonically Increasing Pairwise Sums

Given a list \\(A = [a_1, a_2, ..., a_n]\\), how can one generate all
\\(s\\), where \\(s = a_i + a_j\\) and \\(s \le k\\)?

The first order of business is sorting \\(A\\) because without some
order in \\(A\\), we can't possibly produce monotonically increasing
sums.

A sample grid to help visualize the problem:

|          |          |          |          |          |          |          |          |
|     ---- |     ---- |     ---- |     ---- |     ---- |     ---- |     ---- |     ---- |
|        - |    **1** |    **4** |    **5** |    **8** |  **100** | **1000** | **1001** |
|    **1** |    **2** |        5 |        6 |        9 |      101 |     1001 |     1002 |
|    **4** |        5 |    **8** |        9 |       12 |      104 |     1004 |     1005 |
|    **5** |        6 |        9 |   **10** |       13 |      105 |     1005 |     1006 |
|    **8** |        9 |       12 |       13 |   **16** |      108 |     1008 |     1009 |
|  **100** |      101 |      104 |      105 |      108 |  **200** |     1100 |     1101 |
| **1000** |     1001 |     1004 |     1005 |     1008 |     1100 | **2000** |     2001 |
| **1001** |     1002 |     1005 |     1006 |     1009 |     1101 |     2001 | **2002** |

The grid is reflected across the diagonal because addition is
commutative (\\( a + b = b + a\\)). So as far as unique sums are
concerned exploring either the upper or lower triangle should lead to
the same result.

For convenience, evaluating the lower triangle. On each row, the max is
in the right-most cell, and the min is in the left-most cell. However,
given rows \\(r_i\\) and \\(r_{i+1}\\), it's not always the case that
all of \\(r_i\\)'s cells have lower values than those in \\(r_{i+1}\\)'s
cells.

While I can't devise a monotonically increasing traversal path in the
grid, there are perf improvements to be made from limiting what we
evaluate on row of the lower triangle.

{{% comment %}}

The usage of generators tripped me up:

```py
def unsorted_gen():
  for x in (1, 4, 3, 2, 5):
    yield x

def sorted_gen():
  for x in range(1, 6):
    yield x

def pairs_with_replacement(iterable):
  for a in iterable:
    for b in iterable:
      yield (a, b)

if __name__ == "__main__":
  print(len(list(pairs_with_replacement(sorted_gen())))) # 6
  print(len(list(pairs_with_replacement(unsorted_gen())))) # 4
  print(len(list(pairs_with_replacement(sorted(unsorted_gen()))))) # 25
```

In hindsight, this makes sense because generators visit any given value
at most once. However, given a generator, `pairs_with_replacement` tries
to loop over it twice, which is impossible. This code avoids the bug:

```py
def pairs_with_replacement(iterable):
  iterable = sorted(iterable)
  for a in iterable:
    for b in iterable:
      yield (a, b)
```

The additional memory usage is necessary for correctness.

TIL that `range` returns a `list`, while `xrange` returns a generator,
and therefore `xrange` is more memory-efficient.

{{% cite PythonGenerators %}}

{{% /comment %}}

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/023-non-abundant-sums/non_abundant_sums_deluxe.py`
  highlight="python"
  id="PE023PyDeluxe" >}}

The perf profile shows that the total time in `pairwise_sums` decreased
by \\(.915s \approx 27\\%\\). The additional `sort` call was not even
expensive.

```log
4179871.0
         12183910 function calls in 3.554 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    2.442    2.442    3.552    3.552 non_abundant_sums_deluxe.py:8(pairwise_sums)
 12148815    0.931    0.000    0.931    0.000 {method 'add' of 'set' objects}
    28122    0.172    0.000    0.172    0.000 non_abundant_sums.py:8(sum_of_proper_divisors)
     6966    0.006    0.000    0.178    0.000 non_abundant_sums.py:29(generate_abundant_nums)
        1    0.002    0.002    3.554    3.554 non_abundant_sums_deluxe.py:33(sum_of_non_abundant_sums)
        1    0.001    0.001    0.179    0.179 {built-in method builtins.sorted}
        1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
        1    0.000    0.000    3.554    3.554 {built-in method builtins.exec}
        1    0.000    0.000    3.554    3.554 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
```

Not the \\(50\\%\\) boost I anticipated, but \\(27\\%\\) is good enough?
I wasn't expecting the code to be an order of magnitude faster.

## Learning from Others

{{% cite HackerRankPE023 %}} has a variation of this problem, where
given a list of numbers, print `YES` if the number can be written as the
sum of two abundant numbers, and `NO` otherwise. Caching
`abundant_sums_less_than_K` is important to avoid repeated work.

```py
if __name__ == "__main__":
  K = 28123
  abundant_sums_less_than_K = pairwise_sums(generate_abundant_nums(1, K - 1), K)

  num_test_cases = int(input().strip())
  for _ in range(num_test_cases):
      N = int(input().strip())
      if N > K or N in abundant_sums_less_than_K: print("YES")
      else: print("NO")
```

{{% cite WolframAbundantNumber %}} notes that every number greater than
\\(20,161\\) can be expressed as a sum of two abundant numbers. This
improves on {{% cite ProjectEuler023 %}}'s floor of \\(28,123\\). With
this info, we go down to \\(1.833s\\) and \\(6,282,406\\) function
calls.

{{% cite ProjectEuler023ThreadTherryka %}}'s Python solution runs in
\\(0.512s\\) (\\(-85.59\\%\\)), and with \\(140,563\\) function calls
(\\(-98.85\\%\\)). The special sauce is in avoiding `pairwise_sums`:

{{< readfile
  file=`content/computer-science/programming-challenges/project-euler/023-non-abundant-sums/non_abundant_sums_deluxe_therryka.py`
  highlight="python"
  id="PE023PyDeluxeTherryka" >}}

The perf is comparable to {{% cite ProjectEuler023ThreadTherryka %}}:

```log
4179871.0
         35094 function calls in 0.578 seconds

   Ordered by: internal time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.395    0.395    0.578    0.578 non_abundant_sums_deluxe_therryka.py:8(sum_of_non_abundant_sums)
    28122    0.176    0.000    0.176    0.000 non_abundant_sums.py:8(sum_of_proper_divisors)
     6966    0.006    0.000    0.182    0.000 non_abundant_sums.py:29(generate_abundant_nums)
        1    0.001    0.001    0.183    0.183 {built-in method builtins.sorted}
        1    0.000    0.000    0.578    0.578 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
        1    0.000    0.000    0.578    0.578 {built-in method builtins.exec}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
```

{{% comment %}}

{{% cite ProjectEuler023ThreadTherryka %}} did not cross my mind at all.
Probably because I tunnel-visioned myself into thinking about
monotonically increasing grid traversals? That sounded math-like, and
given I'm on Project Euler, the more math the better?

{{% /comment %}}

## References

1. {{< citation
  id="PythonImportLib"
  title="importlib — The implementation of import — Python 3.11.2 documentation"
  url="https://docs.python.org/3/library/importlib.html#importing-a-source-file-directly"
  accessed="2023-02-19" >}}

1. {{< citation
  id="PythonGenerators"
  title="Generators - Python Wiki"
  url="https://wiki.python.org/moin/Generators"
  accessed="2023-02-20" >}}

1. {{< citation
  id="HackerRankPE023"
  title="Project Euler #23: Non-abundant sums | HackerRank"
  url="https://www.hackerrank.com/contests/projecteuler/challenges/euler023/problem"
  url_2="https://www.hackerrank.com/contests/projecteuler/challenges/euler023/submissions/code/1356679785"
  accessed="2023-02-20" >}}

1. {{< citation
   id="WolframAbundantNumber"
   title="Abundant Number -- from Wolfram MathWorld"
   url="https://mathworld.wolfram.com/AbundantNumber.html"
   accessed="2023-02-20" >}}

1. {{< citation
  id="ProjectEuler023ThreadTherryka"
  title="Thread 23 - Project Euler"
  url="https://projecteuler.net/thread=23;page=8#411899"
  accessed="2023-02-20" >}}

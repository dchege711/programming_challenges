---
date: 2023-02-19
domains:
- docs.python.org
- projecteuler.net
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
  file=`content/computer-science/programming-challenges/project-euler/023-non-abundant-sums/brute_force_non_abundant_sums.py`
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

## References

1. {{< citation
  id="PythonImportLib"
  title="importlib — The implementation of import — Python 3.11.2 documentation"
  url="https://docs.python.org/3/library/importlib.html#importing-a-source-file-directly"
  accessed="2023-02-19" >}}

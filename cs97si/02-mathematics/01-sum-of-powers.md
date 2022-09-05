---
cited-authors:
- Park, Jaehyun
date: '2020-06-20'
domains:
- brilliant.org
- stanford.edu
local_url: http://localhost:1313/computer-science/programming-challenges/cs97si/02-mathematics/01-sum-of-powers/
title: 01. Sum of Powers
---

The sums of powers, \\( \sum_{k=1}^{n} k^a \\), can be computed more efficiently if we have a closed formula for them.

## Sum of powers for a = 1, 2, 3

$$ \sum_{k=1}^{n} k = \frac{n(n+1)}{2} $$

{{% comment %}}

One [intuitive] derivation is presented at brilliant.org {{% cite brilliantFaulhaber %}}:

$$ S_n = 1 + 2 + 3 + ... + n $$

... can be reordered as:

$$ S_n = n + (n-1) + (n-2) + ... + 1 $$

... and if we add the two equations above:

$$ 2 \cdot S_n = (n + 1) + (2 + (n-1)) + (3 + (n-2)) + ... + (n + 1) $$
$$ 2 \cdot S_n = (n + 1) + (n + 1) + (n + 1) + ... + (n + 1) $$
$$ S_n = \frac{n(n+1)}{2} $$

{{% /comment %}}

$$ \sum_{k=1}^{n} k^2 = \frac{n(n+1)(2n + 1)}{6} $$

{{% comment %}}

The sum re-arrangement technique for proving \\( \sum_{k=1}^{n} k \\) does not shed any light here. The folks at brilliant.org show an alternate proof that helps {{< cite brilliantFaulhaber >}}:

$$ (k−1)^3 = k^3 - 3k^2 + 3k - 1 $$
$$ \therefore k^3 - (k-1)^3 = 3k^2 - 3k + 1 $$
$$ \sum_{k=1}^{n} \left( k^3 - (k-1)^3 \right) = \sum_{k=1}^{n} \left( 3k^2 - 3k + 1 \right) $$

The above form is convenient because the LHS telescopes, and the only unknown on the RHS is \\(\sum k^2\\), i.e.

$$ n^3 = 3 \sum k^2 - 3 \left( \frac{n(n+1)}{2} \right) + n $$

From this point onwards, isolating \\( \sum k^2 \\) should give \\( \frac{n(n+1)(2n + 1)}{6} \\).

{{% /comment %}}

$$ \sum_{k=1}^{n} k^3 = \left( \sum_{k=1}^{n} k \right) ^2 = \left( \frac{n(n+1)}{2} \right)^2 $$

{{% comment %}}

I don't follow the jump from \\(\sum_{k=1}^{n} k^3\\) to \\(\left( \sum_{k=1}^{n} k \right) ^2 \\)

{{% /comment %}}

## General Formula

{{% comment %}}

If we wish to compute sums of consecutive powers, Faulhaber's formula provides a neat way to do it {{% cite brilliantFaulhaber %}}:

$$ \sum_{k=1}^{n} k^a = \frac{1}{a+1} \sum_{j=0}^{a} (-1)^{j} \binom{a+1}{j} B_j n^{a+1-j} $$

where \\(B_j\\) is the \\(j\\)-th Bernoulli number, more specifically, \\(B_{j}^{+}\\) where \\(B_{j}^{1} = \frac{1}{2}\\). The Bernoullis can be generated using:

$$ B\_{j}^{+} = 1 - \sum_{k=0}^{j-1} \binom{j}{k} \frac{B_{k}^{+}} {j - k + 1} $$

{{% /comment %}}

## References

1. {{< citation
    id="cs97siMathematics"
    url="http://stanford.edu/class/cs97si/02-mathematics.pdf"
    title="CS 97SI: 02. Mathematics"
    date="2015-06-29"
    author="Jaehyun Park"
    publisher="Stanford CS Department">}}

1. {{< citation
    id="brilliantFaulhaber"
    url="https://brilliant.org/wiki/sum-of-n-n2-or-n3/"
    title="Sum of n, n², or n³ | Brilliant Math and Science Wiki">}}

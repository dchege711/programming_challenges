---
title: Unique Paths to the Bottom-Right Corner
date: 2022-07-30
---

## Problem

Starting from the top-left corner, what is the number of possible unique
paths to reach the bottom-right corner, if you can only move either down
or right at any point in time? {{% cite LCUniquePaths %}}

## Dynamic Programming Solution for Moving Down/Right

{{% tag dynamic-programming %}}

The number of unique paths to `grid[r][c]` is the number of unique paths
to `grid[r-1][c]` plus the number of unique paths to `grid[r][c-1]`,
e.g.,

<table>
<tbody>
<tr><td>1</td><td>1</td><td>1</td><td>1</td></tr>
<tr><td>1</td><td>2</td><td>3</td><td>4</td></tr>
<tr><td>1</td><td>3</td><td>6</td><td>10</td></tr>
</tbody>
</table>

At any given time, we're interested in two adjacent rows, so our space
usage should be at most \\(2n\\). Furthermore, we do not back-track to
the left, so if we update our values left-to-right, we can use \\(n\\)
space because the current cell will have the value from what would have
been in the previous row.

```cpp
int uniquePaths(int m, int n) {
  if (m <= 0 || n <= 0) return 0;

  std::vector<int> numPathsToPosition(n, 0);
  numPathsToPosition[0] = 1;

  for (int r = 0; r < m; ++r) {
    for (int c = 0; c < n; ++c) {
      if (int nextC = c + 1; nextC < n) {
          numPathsToPosition[nextC] += numPathsToPosition[c];
      }
    }
  }

  return numPathsToPosition[n-1];
}
```

The above solution is pretty fast for {{% cite LCUniquePaths %}}'s
online judge, so the runtime (less than 5ms) is not comparable. The
runtime is \\(O(m n)\\).

The memory usage is \\(O(n)\\). Per {{% cite LCUniquePaths %}}, that's
\\(\approx\\) 6MB which is better than 63% of submissions. The range is
5,400 KB to 6,500 KB, so all submissions within an order of magnitude of
each other.

{{% comment %}}

The answer is guaranteed to be \\(\le 2 \cdot 10^9 = 2^{lg(2 \cdot
10^9)} < 2^{31}\\). The `int` data type is guaranteed to be at least 16
bits, but on 32/64 bit systems, it's almost guaranteed to be at least 32
bits wide {{% cite cppReferenceFundamentalTypes %}}. I can specify
`std:uint32_t` from `<cstdint>` to be sure that a 32-bit representation
is used {{% cite cppReferenceFixWidthIntegerTypes %}}.

`std:uint32_t` is unlikely to improve the space usage as I don't see
solutions that use 3MB. It's more likely that LeetCode's environment
uses 32-bit `int`s. Update: no improvement, both `sizeof(int)` and
`sizeof(std::uint32_t)` evaluate to `4` bytes (and therefore 32 bits).

{{% /comment %}}

## Combinatorics Solution for Moving Down/Right

{{% tag combinatorics %}}

{{% cite Archit91LCUniquePaths %}} models the problem as a combinatorics
problem: to get to the bottom-right, we need to choose \\(m - 1\\) right
moves and \\(n - 1\\) down moves, from a total of \\(m + n - 2\\) moves.

$$ C_{m-1}^{m+n-2} = C_{n-1}^{m+n-2} = \frac{(m+n-2)!}{(m-1)!(n-1)!} $$

{{% comment %}}

Why is \\(C_{m-1}^{m+n-2} = C_{n-1}^{m+n-2}\\) true? In general,
\\(C^{n}_{k} = \frac{n!}{k!(n-k)!}\\), and so this follows
algebraically. It's not a special property of the problem.

{{% /comment %}}

Assuming \\(m > n\\) in order to eliminate \\((m-1)!\\) gives us:

$$ C_{m-1}^{m+n-2} = \frac{(m+n-2) \times (m+n-3) \times ... \times (m+n-n) \times (m-1) \times ... \times 1 }{ ((m-1) \times (m-2) \times ... \times 1) (n-1)!} $$
$$ = \frac{(m+n-2) \times (m+n-3) \times ... \times m}{(n-1)!} $$

{{% open-comment %}}

How is the above guaranteed to be an integer?

By definition, \\(C^{n}_{k}\\) counts something, and therefore it's an
integer. {{% cite SECombinationIsAnInteger %}}

From {{% cite proofWikiBinomialCoefficientInteger %}}:

$$ C^{n}_{k} = \binom{n}{k} = \frac{n}{k!(n-k)!} $$
$$ = \frac{n (n-1) (n-2) ... (n - k + 1)}{k!} $$

... the numerator is a product of \\(k\\) successive integers, and {{%
cite proofWikiBFactorialDividesProductOfSuccessiveNumbers %}} (whose
proof is beyond my mathematical maturity) states that the factorial of
\\(k\\) divides the product of \\(k\\) successive numbers.

The mathematical aspect is settled, but why does the code before work
without running into precision errors? Shouldn't we first compute the
numerator, and then divide out the denominator? Dividing integral types
is pretty bad as it truncates the result.

The largest numerator possible is when \\(m = 100, n = 100\\), which is
\\(198 \cdot 197 \cdot ... \cdot 100 = \frac{198!}{99!} < 2^{712}\\).
The largest ints in {{% cite cppReferenceFundamentalTypes %}} are 64
bits wide, and thus too small to hold \\(2^{712}\\). `long double`'s max
is \\(1.18973 \cdot 10^{4932}\\), which can hold \\(2^{712} < 2.155
\cdot 10^{214}\\), but with loss of precision. To use a `long double`
without losing precision, then we're limited by `LDBL_MANT_DIG`, which
is typically `64`, and thus not better than the biggest `int` from the
standard library. {{% cite cppReferenceNumericLimits %}}

Maybe the takeaway is that if you're worried about large numbers
(factorials and such), use a language that natively supports
infinite-precision arithmetic, e.g. Python?

{{% /open-comment %}}

```cpp
int unique_paths(int m, int n) {
  long num_combinations = 1;
  const int m_prime = max(m, n);
  // Because m' > n', when `num <= m'` all of the values in [1, n') will
  // have been visited by the loop.
  for (int num = m + n - 2, denom = 1; num >= m_prime; --num, ++denom) {
    num_combinations *= num;
    if (denom < n) num_combinations /= denom;
  }
  return num_combinations;
}
```

Re-ordering \\(m, n\\), such that \\(m\\) is the max, the loop goes
through \\(m + n - 2 - m = n - 2\\) iterations, and therefore the
runtime is \\(O(min(m, n))\\). The space usage is \\(O(1)\\). I don't
think we can do better than this.

{{% comment %}}

Curiously, the space usage for the combinatorics approach is 5.9MB. I
thought it'd be much lower than the 6MB used in the DP solution. On
second thought, the DP solution uses \\(O(n)\\) space, and the problem
is defined such that \\(n \le 100\\), which is close to \\(O(1)\\).

{{% /comment %}}

## Takeaways

I was wrong to earlier assume that because other people's solutions were
within one magnitude of 6MB, the DP solution was the optimal one.
Sometimes \\(n\\) is too small to distinguish between optimal and
sub-optimal solutions.

The regularity of the problem (well-defined grid; only moving
right/down) should hint at a (probably optimal) math-based answer.

## References

1. {{< citation
  id="LCUniquePaths"
  title="Unique Paths (Medium) - LeetCode"
  url="https://leetcode.com/problems/unique-paths/"
  url_2="https://leetcode.com/submissions/detail/761078821/"
  url_3="https://leetcode.com/submissions/detail/761262306/"
  accessed="2022-07-30" >}}

1. {{< citation
  id="cppReferenceFundamentalTypes"
  title="Fundamental types - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/types"
  accessed="2022-07-30" >}}

1. {{< citation
  id="cppReferenceFixWidthIntegerTypes"
  title="Fixed width integer types (since C++11) - cppreference.com"
  url="https://en.cppreference.com/w/cpp/types/integer"
  accessed="2022-07-30" >}}

1. {{< citation
  id="Archit91LCUniquePaths"
  title="✅ [C++/Python] 5 Simple Solutions w/ Explanation | Optimization from Brute-Force to DP to Math - LeetCode Discuss"
  url="https://leetcode.com/problems/unique-paths/discuss/1581998/C%2B%2BPython-4-Simple-Solutions-w-Explanation-or-Optimization-from-Brute-Force-to-DP-to-Math"
  accessed="2022-07-30" >}}

1. {{< citation
  id="SECombinationIsAnInteger"
  title="elementary number theory - Proof that a Combination is an integer - Mathematics Stack Exchange"
  url="https://math.stackexchange.com/questions/11601/proof-that-a-combination-is-an-integer"
  accessed="2022-07-31" >}}

1. {{< citation
  id="proofWikiBinomialCoefficientInteger"
  title="Binomial Coefficient is Integer - ProofWiki"
  url="https://proofwiki.org/wiki/Binomial_Coefficient_is_Integer"
  accessed="2022-07-31" >}}

1. {{< citation
  id="proofWikiBFactorialDividesProductOfSuccessiveNumbers"
  title="Factorial Divides Product of Successive Numbers - ProofWiki"
  url="https://proofwiki.org/wiki/Factorial_Divides_Product_of_Successive_Numbers"
  accessed="2022-07-31" >}}

1. {{< citation
  id="cppReferenceNumericLimits"
  title="std::numeric_limits - cppreference.com"
  url="https://en.cppreference.com/w/cpp/types/numeric_limits"
  url_2="https://en.cppreference.com/w/cpp/types/climits"
  accessed="2022-07-31" >}}

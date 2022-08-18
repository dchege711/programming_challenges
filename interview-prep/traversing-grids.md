---
date: 2022-07-30
domains:
- en.cppreference.com
- leetcode.com
- math.stackexchange.com
- proofwiki.org
local_url: http://localhost:1313/computer-science/programming-challenges/interview-prep/traversing-grids/
tags:
- backtracking
- combinatorics
- depth-first-search
- dynamic-programming
title: Traversing Grids
---

{{% priors %}}

\\(M \times N\\) grids are naturally represented as 2-dimensional
arrays. Traversing row-by-row is important as it takes advantage of
locality.

{{% /priors %}}

{{% priors %}}

One can also represent the grid as a one-dimensional arrays, with a
helper function to convert from the 2D coordinates to the corresponding
1D index.

{{% /priors %}}

## Counting Unique Paths

### Moving Down/Right

> Starting from the top-left corner, what is the number of possible
> unique paths to reach the bottom-right corner, if you can only move
> either down or right at any point in time? {{% cite LCUniquePaths %}}

#### Dynamic Programming Solution for Moving Down/Right

{{% tag dynamic-programming %}}

The number of unique paths to `grid[r][c]` is the number of unique paths
to `grid[r-1][c]` plus the number of unique paths to `grid[r][c-1]`.

At any given time, we're interested in two adjacent rows, so our space
usage should be at most \\(2n\\). Furthermore, we do not back-track to
the left, so if we update our values left-to-right, we can use \\(n\\)
space.

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

#### Combinatorics Solution for Moving Down/Right

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

#### Takeaways

I was wrong to earlier assume that because other people's solutions were
within one magnitude of 6MB, the DP solution was the optimal one.
Sometimes \\(n\\) is too small to distinguish between optimal and
sub-optimal solutions.

The regularity of the problem (well-defined grid; only moving
right/down) should hint at a (probably optimal) math-based answer.

### Moving Down/Right With Obstacles

> Starting from the top-left corner, what is the number of possible
> unique paths to reach the bottom-right corner, if you can only move
> either down or right at any point in time, and the path cannot include
> any square that is an obstacle? {{% cite LCUniquePathsII %}}

The addition of obstacles has these implications:

* I can't use the combinatorics formula because the problem is no longer
  easy to define in general terms.
* The logic in the inner loop of the DP solution needs to take the
  obstacles into account.

{{% tag dynamic-programming %}}

```cpp
int uniquePathsWithObstacles(vector<vector<int>>& obstacleGrid) {
  if (obstacleGrid[0][0] == 1) return 0;

  const int m = obstacleGrid.size();
  const int n = obstacleGrid[0].size();

  std::vector<int> numPathsToPosition(n, 0);
  numPathsToPosition[0] = 1;

  for (int r = 0; r < m; r++) {
    for (int c = 0; c < n; c++) {
      if (int nextC = c + 1; nextC < n) {
        if (obstacleGrid[r][nextC] == 1) {
          numPathsToPosition[nextC] = 0;
        } else {
          numPathsToPosition[nextC] += numPathsToPosition[c];
        }
      }

      if (int nextR = r + 1; nextR < m && obstacleGrid[nextR][0] == 1) {
        numPathsToPosition[0] = 0;
      }
    }
  }

  return numPathsToPosition[n-1];
}
```

The time and space usage is the same as the [earlier DP
solution](#dynamic-programming-solution-for-moving-downright):
\\(O(m n)\\) running time, and \\(O(n)\\) space usage. I don't think we
can do better.

{{% comment %}}

Curiously, the reference solution for {{% cite LCUniquePathsII %}} uses
\\(O(1)\\) space by modifying the input `vector<vector<int>>&`. I did
not consider this as an option; I assumed the input should remain
unchanged (despite it not being a const-ref).

{{% /comment %}}

### 4-Directional Spanning Walks

> Given an \\(M \times N\\) integer array `grid` where `grid[i][j]`
> could be:
>
> * `1` representing the starting square. There is exactly one starting square.
> * `2` representing the ending square. There is exactly one ending square.
> * `0` representing empty squares that we can walk over.
> * `-1` representing obstacles that we cannot walk over.
>
> Return the number of 4-directional walks from the starting square to
> the ending square, that walk over every non-obstacle square exactly
> once.

DP no longer seems appropriate for this question because we'd need to
store a lot of information at every cell. Furthermore, given that we can
move in any of the four directions, it's not clear what the DP relation
(e.g. `grid[r][c] ?= grid[r-1][c-1] + ... + grid[r+1][c+1]`) applies.

Of the path traversals in a grid, depth-first-search seems like a better
fit because we can "cache" results through the call stack. The longest
possible path is \\(MN \le 20\\), so we'll not run into a call stack
overflow.

```py
def uniquePathsIII(self, grid: List[List[int]]) -> int:
  visited = [[False] * len(l) for l in grid]
  M = len(grid)
  N = len(grid[0])

  # Find the starting location, and the number of obstacles
  r_origin = -inf
  c_origin = -inf
  num_obstacles = 0
  for r in range(M):
    for c in range(N):
      if grid[r][c] == 1:
        r_origin = r
        c_origin = c
      elif grid[r][c] == -1:
        num_obstacles += 1

  num_non_obstacles = (M * N) - num_obstacles

  # Starting from grid[r][c], do a DFS counting complete spanning paths.
  def dfs(r: int, c: int, num_visited: int) -> int:
    if (grid[r][c] == 2):
      return 1 if num_visited == num_non_obstacles else 0

    num_complete_trips = 0
    for delta in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
      new_r = r + delta[0]
      new_c = c + delta[1]
      is_valid_idx = new_r >= 0 and new_r < M and new_c >= 0 and new_c < N
      if is_valid_idx and not visited[new_r][new_c]:
        if grid[new_r][new_c] != -1:
          visited[new_r][new_c] = True
          num_complete_trips += dfs(new_r, new_c, num_visited + 1)
          visited[new_r][new_c] = False

    return num_complete_trips

  visited[r_origin][c_origin] = True
  return dfs(r_origin, c_origin, 1)
```

{{% tag depth-first-search %}}
{{% tag backtracking %}}

{{% comment %}}

The fact that when we reach an invalid path, we discard the path and try
out other options makes this problem a backtracking problem.

{{% /comment %}}

The running time of DFS is \\(O(V + E)\\), and given that \\(V = MN\\)
and \\(E \le 4 \cdot MN\\), the overall runtime is \\(O(MN)\\).
Computing `r_origin`, `c_origin` and `num_obstacles` is also
\\(O(MN)\\). So the overall runtime is still \\(O(MN)\\). I doubt we can
do better than DFS. On {{% cite LCUniquePathsIII %}}'s leaderboard, the
above solution is faster than 69% of the submissions.

The space usage is \\(O(MN)\\) for the `visited` array. I prefer not to
modify the incoming `grid`. On {{% cite LCUniquePathsIII %}}'s
leaderboard, the above solution uses less memory than 94% of the
submissions. The fact that \\(MN \le 20\\) allows us to optimize space
usage further by using a bit-mask. {{% cite Archit91LCUniquePathsIII %}}

{{% comment %}}

With Python's infinite arithmetic precision, we could presumably use
bit-masking for any \\(MN\\), instead of only using it for \\(MN \le
64\\).

{{% /comment %}}

{{% comment %}}

C++'s `std::bitset` needs to have its number of bits specified at
compile time. For a variable number of bits, `std::vector<bool>` is
recommended. {{% cite cppReferenceBitset %}} In this case though, a
`std::bitset<20> visited` is sufficient.

{{% /comment %}}

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
  id="LCUniquePathsII"
  title="Unique Paths II (Medium) - LeetCode"
  url="https://leetcode.com/problems/unique-paths-ii/"
  url_2="https://leetcode.com/submissions/detail/761291032/"
  accessed="2022-07-31" >}}

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

1. {{< citation
  id="LCUniquePathsIII"
  title="Unique Paths III (Hard) - LeetCode"
  url="https://leetcode.com/problems/unique-paths-iii/submissions/"
  url_2="https://leetcode.com/submissions/detail/762026448/"
  accessed="2022-07-31" >}}

1. {{< citation
  id="Archit91LCUniquePathsIII"
  title="✅ [C++] DFS + Backtracking + Bit Manipulation | Short & Simple w/ Explanation | Beats 100% - LeetCode Discuss"
  url="https://leetcode.com/problems/unique-paths-iii/discuss/1554054/C%2B%2B-DFS-%2B-Backtracking-%2B-Bit-Manipulation-or-Short-and-Simple-w-Explanation-or-Beats-100"
  accessed="2022-07-31" >}}

1. {{< citation
  id="cppReferenceBitset"
  title="std::bitset<N>::bitset - cppreference.com"
  url="https://en.cppreference.com/w/cpp/utility/bitset/bitset"
  accessed="2022-08-02" >}}

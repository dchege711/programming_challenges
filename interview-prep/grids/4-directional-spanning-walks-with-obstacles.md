---
date: 2022-07-31
domains:
- en.cppreference.com
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/interview-prep/grids/4-directional-spanning-walks-with-obstacles/
tags:
- backtracking
- depth-first-search
title: Spanning 4-Directional Walks From Origin to Destination w/ Obstacles
---

## Problem

Given an \\(M \times N\\) integer array `grid` where `grid[i][j]` could
be:

* `1` representing the starting square. There is exactly one starting
  square.
* `2` representing the ending square. There is exactly one ending
  square.
* `0` representing empty squares that we can walk over.
* `-1` representing obstacles that we cannot walk over.

Return the number of 4-directional walks from the starting square to the
ending square, that walk over every non-obstacle square exactly once.

## Solution

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

---
date: 2022-07-31
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/grids/unique-down-right-paths-with-obstacles/
tags:
- dynamic-programming
title: Unique Paths to the Bottom-Right Corner w/ Obstacles
---

## Problem

Starting from the top-left corner, what is the number of possible unique
paths to reach the bottom-right corner, if you can only move either down
or right at any point in time, and the path cannot include any square
that is an obstacle? {{% cite LCUniquePathsII %}}

## Solution

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

The time and space usage is the same as the [earlier DP solution]({{<
ref
"/computer-science/programming-challenges/leet-code-and-others/grids/unique-down-right-paths#dynamic-programming-solution-for-moving-downright"
>}}): \\(O(m n)\\) running time, and \\(O(n)\\) space usage. I don't
think we can do better.

{{% comment %}}

Curiously, the reference solution for {{% cite LCUniquePathsII %}} uses
\\(O(1)\\) space by modifying the input `vector<vector<int>>&`. I did
not consider this as an option; I assumed the input should remain
unchanged (despite it not being a const-ref).

{{% /comment %}}

## References

1. {{< citation
  id="LCUniquePathsII"
  title="Unique Paths II (Medium) - LeetCode"
  url="https://leetcode.com/problems/unique-paths-ii/"
  url_2="https://leetcode.com/submissions/detail/761291032/"
  accessed="2022-07-31" >}}

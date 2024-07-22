---
title: "Shortest Path in a Grid w/ Obstacles Elimination"
date: 2024-07-21
---

{{< citation
  id="leetCode1293"
  title="Shortest Path in a Grid with Obstacles Elimination - LeetCode"
  url="https://leetcode.com/problems/shortest-path-in-a-grid-with-obstacles-elimination/"
  accessed="2024-07-21" >}}

## Problem

You are given an \\(m \times n\\) `grid` where each cell is either `0`
(empty) or `1` (obstacle). You can move up, down, left, or right from
and to an empty cell in one step.

Return the minimum number of steps to walk from the upper left corner
\\((0, 0)\\) to the lower-right corner \\((m-1, n-1)\\) given that you
can eliminate at most \\(k\\) obstacles. If it is not possible to find
such a walk, return `-1`.

{{< figure
  src="/img/computer-science/programming-challenges/leet-code-and-others/grids/shortest-path-w-obstacle-elimination/5x3-grid.jpg"
  caption="\\([[0,0,0],[1,1,0],[0,0,0],[0,1,1],[0,0,0]]\\) with \\(k=1\\). The shortest path with one obstacle elimination at position \\((3, 2)\\) is 6." >}}

Constraints:

* `m == grid.length`
* `n == grid[i].length`
* \\(1 \le m, n \le 40\\)
* \\(1 \le k \le mn \\)
* `grid[i][j]` is either `0` or `1`.
* `grid[0][0] == grid[m - 1][n - 1] == 0`

## Solution

Can I model this as a DP problem? The shortest path to `grid[r][c]` is
the \\(1\\) plus shortest path to any of the 4 ways at getting to
`grid[r][c]`.

<details>
<summary>DP attempt</summary>

```py
def shortest_path_in_grid_with_obstacles_elimination(grid: List[List[int]], K: int) -> int:
    R = len(grid)
    assert R > 0, "There should be at least one row"

    C = len(grid[0])
    assert all(len(row) == C for row in grid), f"All rows should have {C} columns"

    assert grid[0][0] == 0, "(0, 0) should not have an obstacle"
    assert grid[R - 1][C - 1] == 0, "Destination should not have an obstacle"

    possible_steps = [Step(0, -1), Step(0, 1), Step(1, 0), Step(-1, 0)]

    def in_range(r, c):
        return r >= 0 and c >= 0 and r < R and c < C

    def fewest_steps_to_point(r: int, c: int, k: int, visited: set):
        assert (r, c) not in visited, f"Should not revisit ({r}, {c})"
        visited.add((r, c))

        # We start from (0, 0). No steps needed to get here.
        if r == c and c == 0:
            return 0

        # If it's impossible to get here, return `inf`.
        if grid[r][c] == 1 and k < 0:
            return inf

        # Consider all the ways that we could have gotten to (r, c). Pick the
        # one with the fewest number of steps
        fewest_steps = inf
        for dr, dc in possible_steps:
            new_r, new_c = r + dr, c + dc
            if not in_range(new_r, new_c):
                continue

            if (new_r, new_c) in visited:
                continue

            is_empty_cell = grid[new_r][new_c] == 0
            steps = fewest_steps_to_point(
                new_r, new_c, k if is_empty_cell else k - 1, visited
            )

            if steps < fewest_steps:
                fewest_steps = steps

        return fewest_steps + 1

    fewest_steps_to_dest = fewest_steps_to_point(R - 1, C - 1, K, set())
    return fewest_steps_to_dest if fewest_steps_to_dest != inf else -1
```

</details>

But this ignores the fact that the shortest path to `grid[r][c]` might
have used up \\(k\\). A longer path might have conserved \\(k\\) and
used it to create a better shortcut further down.

What about modeling it as a DFS problem? Let the `dfs` succeed if it
gets to `grid[m -1][n - 1]`. When collecting the `dfs` results, discard
the longer path. That way, \\(k\\) is implicitly taken care of because a
successful `dfs` must have reached the `grid[m -1][n - 1]`, and we don't
need to minimize \\(k\\).

<details>

<summary>
DFS attempt that returns a path, not necessarily the shortest one
</summary>

```py
def shortest_path_in_grid_with_obstacles_elimination(grid: List[List[int]], K: int) -> int:
    R = len(grid)
    assert R > 0, "There should be at least one row"

    C = len(grid[0])
    assert all(len(row) == C for row in grid), f"All rows should have {C} columns"

    assert grid[0][0] == 0, "(0, 0) should not have an obstacle"
    assert grid[R - 1][C - 1] == 0, "Destination should not have an obstacle"

    possible_steps = [Step(0, -1), Step(0, 1), Step(1, 0), Step(-1, 0)]

    def in_range(r, c):
        return r >= 0 and c >= 0 and r < R and c < C

    def has_obstacle(r, c):
        return grid[r][c] == 1

    def dfs(r: int, c: int, k: int, visited: set):
        assert (r, c) not in visited, f"Should not revisit ({r}, {c})"
        visited.add((r, c))

        # If we've gotten to the destination, return zero. The path length will
        # be computed as the DFS returns.
        if r == R - 1 and c == C - 1:
            return 0

        # Advance the DFS in all possible unvisited directions
        fewest_steps = inf
        for dr, dc in possible_steps:
            new_r, new_c = r + dr, c + dc

            if not in_range(new_r, new_c): continue
            if (new_r, new_c) in visited: continue

            new_k = k - 1 if has_obstacle(new_r, new_c) else k
            if new_k < 0: continue

            steps = dfs(new_r, new_c, new_k, visited, depth + 1)
            fewest_steps = min(steps, fewest_steps)

        return fewest_steps + 1

    fewest_steps_to_dest = dfs(0, 0, K, set(), 0)
    return fewest_steps_to_dest if fewest_steps_to_dest != inf else -1
```

While I can find *a* path, the above doesn't give me the shortest path.

</details>

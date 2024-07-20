---
title: Grid Game
date: 2024-07-20
---

{{< citation
  id="gridGame"
  title="Grid Game - LeetCode"
  url="https://leetcode.com/problems/grid-game/description/"
  accessed="2024-07-20" >}}

## Problem

You are given a 0-indexed 2D array `grid` of size \\(2 \times n\\),
where `grid[r][c]` represents the number of points at position \\((r,
c)\\) on the matrix. Two robots are playing a game on this matrix.

Both robots initially start at \\((0, 0)\\) and want to reach \\((1,
n-1)\\). Each robot may only move to the right or down.

At the start of the game, the first robot moves from \\((0, 0)\\) to
\\((1, n-1)\\), collecting all the points from the cells on its path.
For all cells \\((r, c)\\) traversed on the path `grid[r][c]` is set to
\\(0\\). Then, the second robot moves from \\((0, 0)\\) to \\((1,
n-1)\\), collecting the points on its path. Note that their paths may
intersect with one another.

The first robot wants to minimize the number of points collected by the
second robot. In contrast, the second robot wants to maximize the number
of points it collects. If both robots play optimally, return the number
of points collected by the second robot.

{{< figure
  src="/img/computer-science/programming-challenges/interview-prep/grids/grid_game/sample_input.png"
  caption="The input grid is \\([[2, 5, 4], [1, 5, 1]]\\). The first robot takes the optimal path in red resulting in \\([[0, 0, 4], [1, 0, 0]]\\). The second robot will take the optimal path collecting \\(0 + 0 + 4 + 0 = 4\\)." >}}

Constraints:

* `grid.length == 2`
* `n == grid[r].length`
* \\(1 \le n \le 5 \cdot 10^4\\)
* \\(1 \le \texttt{grid[r][c]} \le 10^5\\)

## Buggy Implementation

Given a `grid`, computing the maximum score that we can collect is a
dynamic programming problem. At each `grid[r][c]`, we pick the higher
score from either the path that led us to `grid[r-1][c]` or
`grid[r][c-1]`. This technique solves the second part of the problem.

Is minimizing \\(R_2\\)'s score equivalent to maximizing \\(R_1\\)'s
score? Suppose there are \\(P\\) paths to \\((1, n-1)\\), where some
path \\(p_{max}\\) has the highest score. \\(R_1\\) needs to take away
\\(p_{max}\\) but is that sufficient? Can \\(R_1\\) choose a path that
makes \\(p_{max}\\) impossible for \\(R_2\\) without necessarily taking
\\(p_{max}\\) itself?

Maximizing \\(R_1\\)'s score is not the optimal strategy as shown by
this failing code:

<details>
<summary>Buggy Implementation</summary>

{{< readfile
  file="content/computer-science/programming-challenges/interview-prep/grids/grid_game.py"
  highlight="py" >}}

</details>

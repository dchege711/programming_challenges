---
title: Queen's Attack II
date: 2024-07-20
---

{{< citation
  id="hrQueensAttackII"
  title="Queen's Attack II"
  url="https://www.hackerrank.com/challenges/queens-attack-2/problem"
  accessed="2024-07-20" >}}

## Problem

A queen is standing on an \\(n \times n\\) chess board. The chess
board's rows are numbered from \\(1\\) to \\(n\\), going from bottom to
top. Its columns are numbered from \\(1\\) to \\(n\\), going from left
to right. Each square is referenced by a tuple \\((r, c)\\), describing
the row, \\(r\\), and column, \\(c\\), where the square is located.

The queen is standing at position \\((r_q, c_q)\\). In a single move,
she can attack any square in any of the eight directions (left, right,
up, down, and the four diagonals). However, there are obstacles on the
chessboard, each preventing the queen from attacking any square beyond
it on that path.

{{< figure
  src="/img/computer-science/programming-challenges/interview-prep/grids/queens_attack_ii/sample_5x5.png"
  caption="The queen at \\((4, 3)\\) on a \\(5 \times 5\\) chess board obstacles at \\((2, 3)\\), \\((4, 2)\\), and \\((5, 5)\\). The queen can attack \\(10\\) positions." >}}

Given the queen's position and the locations of all the \\(k\\)
obstacles, find the number of squares that the queen can attack from her
position at \\((r_q, c_q\\)).

Constraints:

* \\(0 < n \le 10^5\\)
* \\(0 \le k \le 10^5\\)
* A single cell may contain more than one obstacle.
* There will never be an obstacle at the position where the queen is
  located.

## Solution

Starting from \\((r_q, c_q\\)), expand in each of the 8 directions until
we run into an obstacle. In each loop, we choose the smallest update in
that direction. This approach would traverse the list \\(8\\) times.
Running time is \\(\mathcal{O}(k)\\).

We can also approach the problem from the outside shrinking in. Start by
assuming that there are no obstacles, and then update the bounds in a
single pass through the \\(k\\) obstacles. Running time is
\\(\mathcal{O}(k)\\). I prefer this over the former as the former might
have less cache locality when looping back from the beginning.

<details>
<summary>Implementation</summary>

{{< readfile
  file="content/computer-science/programming-challenges/interview-prep/grids/queens_attack_ii.py"
  highlight="py" >}}

</details>

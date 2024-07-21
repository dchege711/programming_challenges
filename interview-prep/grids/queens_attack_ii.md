---
date: 2024-07-20
domains:
- www.hackerrank.com
local_url: http://localhost:1313/computer-science/programming-challenges/interview-prep/grids/queens_attack_ii/
title: Queen's Movements on a Chessboard w/ Obstacles
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

We can't do better than \\(\mathcal{O}(k)\\) because we do need to see
each obstacle at least once.

<details>
<summary>Implementation</summary>

{{< readfile
  file="content/computer-science/programming-challenges/interview-prep/grids/queens_attack_ii.py"
  highlight="py" >}}

</details>

Tripping points:

* The indices are 1-based, and not 0-based. Had some off-by-one errors
  when computing the number of squares attackable in a given direction.
* Messing up the update logic. Adding the assertion at the end of the
  loop body that all 8 values are non-negative helped pinpoint where the
  error was.

## Learnings from Editorial

{{% cite hrQueensAttackIIEditorial %}} features an algorithm that
expands out. When parsing the obstacle \\((r, c)\\), they flag it in a
`map<pair<int, int>, int>`, e.g., `mp[{x, 1}] = 1`. For each of the eight
starting positions, they get the number of free cells along that
direction:

```cpp
bool in_range(int x, int y) {
  return x <= n && x > 0 && y <= n && y > 0;
}

int num_free_cells(int x, int y, int dx, int dy) {
  int ans = 0;
  while(in_range(x, y) && !mp[{x, y}]) {
    x += dx;
    y += dy;
    ans++;
  }
  return ans;
}
```

The `std::map` is not strictly necessary as we only need a presence
check. `std::set` would have sufficed. `num_free_cells`'s runtime is
\\(\mathcal{O}(n)\\). This approach uses \\(\mathcal{O}(k)\\) space. Why
does {{% cite hrQueensAttackIIEditorial %}} consider this an
\\(\mathcal{O}(n)\\) algorithm? They still need to parse the \\(k\\)
obstacles, and so shouldn't it be \\(\mathcal{O}(k + n)\\)?

## References

1. {{< citation
  id="hrQueensAttackIIEditorial"
  title="Queen's Attack II > Editorial"
  url="https://www.hackerrank.com/challenges/queens-attack-2/editorial"
  accessed="2024-07-20" >}}

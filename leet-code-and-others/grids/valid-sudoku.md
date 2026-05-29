---
date: 2026-05-29
domains:
- learn.microsoft.com
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/grids/valid-sudoku/
title: Valid Sudoku
---

Determine if a \\(9 \times 9\\) Sudoku board is valid. A Sudoku board has nine
\\(3 \times 3\\) sub-boxes. Validate the filled cells such that each row,
column, and sub-box contain the digits \\([1, ..., 9]\\) without repetition. {{%
cite LCValidSudoku %}}

{{% comment %}}

Unintuitive to me that given a `char c`, we need `c - '0'` to convert it into an
`int`. `Convert.ToInt32('4')` gives us `52`, not `4`. {{% cite Convert.ToInt32
%}}

{{% /comment %}}

I don't think we can do better than validating all 9 rows, 9 columns, and 9
sub-boxes. There's no repeated work that can be optimized. At its core, this
problem is about traversing the grid in different ways. There is room for
improvement in how we handle each check. From the `char[][]` grid, we can use a
`HashSet<char>` to check for duplicates (ignoring `c == '.'`). If we convert the
`char[][]` to an `int?[9,9]` grid, we can use a compact `BitArray` to check for
duplicates, or as I've just learned, a `BitVector32` which is even more compact
at 32 bits.

<details>
<summary>Implementation Using <code>BitVector32</code></summary>

{{< readfile
  file="content/computer-science/programming-challenges/leet-code-and-others/grids/ValidSudoku.cs"
  highlight="cs" >}}

</details>

1. {{< citation
  id="LCValidSudoku"
  title="Valid Sudoku - LeetCode"
  url="https://leetcode.com/problems/valid-sudoku/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-29" >}}

1. {{< citation
  id="Convert.ToInt32"
  title="Convert.ToInt32 Method (System) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.convert.toint32?view=net-10.0#system-convert-toint32(system-char)"
  accessed="2026-05-29" >}}

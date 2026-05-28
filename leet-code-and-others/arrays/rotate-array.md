---
date: 2026-05-28
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/arrays/rotate-array/
title: Rotate Array
---

Given an integer array of length \\(N\\), rotate the array to the right by
\\(k\\) steps, where \\(k \ge 0\\). {{% cite LCRotateArray %}}

Rotation is periodical, e.g., rotating a 5-item array 13 times yields the same
result as rotating it \\( 13 \mod 5 = 3\\) times. So our effective \\(k\\) is
\\(k \mod N\\). That \\(k \ge 0\\) saves us from [worrying about how the
programming language implements modulo operations]({{< ref
"/computer-science/programming-challenges/advent-of-code/2024/AoC2024/14-restroom-redoubt/14-restroom-redoubt#WikiModulo"
>}}).

A brute-force algorithm would have \\(\mathcal{O}(kN)\\) shift operations, where
we iterate over the array \\(k\\) times, and shift the items over once in each
iteration. That said, we'd use no extra space.

If we allow ourselves \\(\mathcal{O}(k)\\) extra space, then we can store the
\\(k\\) elements that will be displaced, use exactly \\(N\\) shift operations:

```cs
public void Rotate(int[] nums, int k) {
  int N = nums.Length;
  k = k % N;

  int[] stash = new int[k];
  int offset = N - k;
  for (int i = offset; i < N; i++)
    stash[i - offset] = nums[i];

  for (int i = N - 1; i >= k; i--)
    nums[i] = nums[i - k];

  for (int i = 0; i < k; i++)
    nums[i] = stash[i];
}
```

How can we solve this problem in \\(\mathcal{O}(1)\\) extra space and less than
\\(\mathcal{O}(kN)\\) shift operations?

1. {{< citation
  id="LCRotateArray"
  title="Rotate Array - LeetCode"
  url="https://leetcode.com/problems/rotate-array/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-28" >}}

---
date: 2026-05-28
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/strings-and-1d-arrays/rotate-array/
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

A brute-force algorithm would have \\(\mathcal{O}(kN)\\) swap operations, where
we iterate over the array \\(k\\) times, and shift the items over once in each
iteration. That said, we'd use no extra space.

If we allow ourselves \\(\mathcal{O}(k)\\) extra space, then we can store the
\\(k\\) elements that will be displaced, use \\(N\\) swap operations:

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
\\(\mathcal{O}(kN)\\) swaps?

When dealing with these kinds of problems, think of ordering and subsequences.
There are only so many primitives to work with. Sorting the array isn't useful
here because we need to keep the relative order of the input array. This leaves
us with reversing operations. Given \\([1, 2, 3, 4, 5, 6, 7]\\) and \\(k = 3\\),
we want to get to \\([5, 6, 7, 1, 2, 3, 4]\\). Reversing the array gives us
\\([7, 6, 5, 4, 3, 2, 1]\\), where the \\([7, 6, 5]\\) and \\([4, 3, 2, 1]\\)
sub-arrays are in the proper positions, but the elements therein are not.
Reversing the sub-arrays themselves gives us \\([5, 6, 7, 1, 2, 3, 4]\\). Viola!

```cs
public void Rotate(int[] nums, int k) {
  int N = nums.Length;
  k = k % N;

  if (k == 0 || N <= 1)
    return;

  ReverseSection(nums, 0, N-1);
  ReverseSection(nums, 0, k - 1);
  ReverseSection(nums, k, N-1);
}

private void ReverseSection(int[] nums, int start, int end)
{
  if (end < start)
    throw new InvalidOperationException($"Violation: start({start}) <= end({end})");

  int temp;
  for (int i = 0; i <= (end - start) / 2; i++)
  {
    temp = nums[start + i];
    nums[start + i] = nums[end - i];
    nums[end - i] = temp;
  }
}
```

1. {{< citation
  id="LCRotateArray"
  title="Rotate Array - LeetCode"
  url="https://leetcode.com/problems/rotate-array/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-28" >}}

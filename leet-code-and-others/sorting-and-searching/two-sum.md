---
date: 2026-05-28
domains:
- introcs.cs.princeton.edu
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/two-sum/
title: Two Sum
---

Given a 1-indexed array of integers that is sorted in non-decreasing order, find
two numbers such that they add up to a `target` number. {{% cite LCTwoSumII %}}

At its core, the problem is a binary search one. Given \\(a\\), find \\(b = t -
a\\) in the right side of `array`. If \\(a > \lfloor t / 2 \rfloor\\), then
there is no possible \\(b\\) to the right because \\(b \ge a\\) and \\(2a >
t\\).

```cs
public int[] TwoSum(int[] numbers, int target) {
  int maxAddend = target / 2;
  for (int i = 0; i < numbers.Length; i++)
  {
    if (numbers[i] > maxAddend)
      throw new InvalidOperationException($"No solution can be found. {numbers[i]} > {maxAddend}");

    var addendIndex = BinarySearch(i + 1, numbers.Length, target - numbers[i]);
    if (addendIndex >= 0)
        return [i + 1, addendIndex + 1];
  }

  throw new InvalidOperationException("No solution can be found. Exhausted all of the numbers");
}
```

The crux of this problem is implementing binary search correctly. My attempt is
buggy:

```cs
int BinarySearch(int lo, int hi, int val)
{
    if (lo > hi) // Bug
        return -1;

    int mid = lo + (hi - lo) / 2;
    int curr = numbers[mid];
    if (val < curr)
        return BinarySearch(lo, mid - 1, val); // Bug
    else if (val > curr)
        return BinarySearch(mid + 1, hi, val);
    else
        return mid;
}
```

Learning from {{% cite COS126BinarySearch %}} (took this class as an undergrad).
Compute \\(mid\\) as \\(lo + (hi - lo) / 2\\) instead of \\((hi + lo) / 2\\) as
the former avoids overflow. Be consistent in the boundary, e.g., with \\([lo,
hi)\\), branching left should use \\([lo, mid)\\) and not \\([lo, mid-1)\\).
Similarly, the base case should have \\(lo \ge hi\\) because \\(hi\\) should
never be a candidate, \\(\ge\\), not \\(>\\).

1. {{< citation
  id="LCTwoSumII"
  title="Two Sum II - Input Array Is Sorted - LeetCode"
  url="https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-28" >}}

1. {{< citation
  id="COS126BinarySearch"
  title="BinarySearch.java"
  url="https://introcs.cs.princeton.edu/java/42sort/BinarySearch.java.html"
  accessed="2026-05-28" >}}

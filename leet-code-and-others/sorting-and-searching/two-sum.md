---
date: 2026-05-28
domains:
- introcs.cs.princeton.edu
- leetcode.com
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/two-sum/
tags:
- binary-search
- two-pointer
title: Two Sum
---

## Problem Description

Given a 1-indexed array of integers that is sorted in non-decreasing order, find
two numbers such that they add up to a `target` number. {{% cite LCTwoSumII %}}

## Solution: Linear Scan with Binary Search

{{% tag binary-search %}}

At its core, the problem is a binary search one. Given \\(a\\), find \\(b = t -
a\\) in the right side of `array`. If \\(a > \lfloor t / 2 \rfloor\\), then
there is no possible \\(b\\) to the right because \\(b \ge a\\) and \\(2a >
t\\).

<details>
<summary>Implementation: Linear Scan with Binary Search Stubbed Out</summary>

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

</details>

The crux of this problem is implementing binary search correctly. My attempt is
buggy:

<details>
<summary>Implementation: Buggy Binary Search</summary>

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

</details>

Learning from {{% cite COS126BinarySearch %}} (took this class as an undergrad).
Compute \\(mid\\) as \\(lo + (hi - lo) / 2\\) instead of \\((hi + lo) / 2\\) as
the former avoids overflow. Be consistent in the boundary, e.g., with \\([lo,
hi)\\), branching left should use \\([lo, mid)\\) and not \\([lo, mid-1)\\).
Similarly, the base case should have \\(lo \ge hi\\) because \\(hi\\) should
never be a candidate, \\(\ge\\), not \\(>\\).

## Solution: Linear Scan with Two Pointers

{{% tag two-pointer %}}

{{% comment %}}

I think I stopped too soon here by associating "sorted array" with binary search
and assuming that's what the interviewer would like to see. Sometimes sorted
array means that a linear scan algorithm like {{% tag two-pointer %}} would
work.

The brute force solution takes \\(\mathcal{O}(N^2)\\) time by considering all
possible pairs. Rephrasing this as a problem for reducing the number of pairs
considered could have also steered me away from binary search, as that involves
pairs that don't exist.

{{% /comment %}}

<details>
<summary>Implementation: Linear Scan with Two Pointers</summary>

```python
class Solution:
    def twoSum(self, numbers: List[int], target: int) -> List[int]:
        left, right = 0, len(numbers) - 1
        while left < right:
            curr_sum = numbers[left] + numbers[right]
            if curr_sum == target:
                return [left + 1, right + 1] # Problem wants 1-indexed indices
            elif curr_sum < target:
                left += 1 # Only hope of increasing next curr_sum
            else:
                right -= 1 # Only hope of reducing next curr_sum

        raise ValueError(f"No pair found that sums to {target}")
```

</details>

If `curr_sum < target`, then we can only increase the next iterations `curr_sum`
by advancing `left` (similar rationale for `curr-sum > target`). {{% cite
TwoPointerHelloInterview %}}

There is no need to consider pairs that would not work. For example, if
`curr_sum < target` decrementing `left` would only reduce `curr_sum`. Increasing
`right` would increase `curr_sum`, but the starting conditions, `right = N - 1`,
ensure that we'd already considered such a pair.

## References

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

1. {{< citation
  id="TwoPointerHelloInterview"
  title="Two-Pointer Overview | Hello Interview"
  url="https://www.hellointerview.com/learn/code/two-pointers/overview"
  accessed="2026-06-06" >}}

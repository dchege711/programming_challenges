---
date: 2026-05-27
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/merging-sorted-arrays/
title: Merging Sorted Arrays
---

## Merge Sorted Arrays In-Place With Buffer

You are given two integer arrays `nums1` and `nums2`, sorted in non-decreasing
order, and two integers `m` and `n` representing the number of elements in
`nums1` and `nums2` respectively. `nums1` has a length of \\(m + n\\). Merge
`nums1` and `nums2` into `nums1` in non-decreasing order. {{% cite
LCMergeSortedArray %}}

We don't want to use extra \\(\mathcal{O}(m + n)\\) space. Traversing `nums1`
and `nums2` from left to right creates complications, e.g., given \\([4, 6, 8,
0, 0, 0]\\) and \\([3, 4, 5]\\), the intermediate \\([3, 6, 8, 0, 0, 0]\\)
raises questions on where to stash the \\(4\\). However, starting from \\(m -
1\\) and \\(n - 1\\) leads to an elegant solution where we don't need
intermediate stashes.

{{% comment %}}

Think of how the direction of traversal can help. There are only so many ways of
traversing a sorted data structure -- think through the possible options before
committing to a path forward.

{{% /comment %}}

## References

1. {{< citation
  id="LCMergeSortedArray"
  title="Merge Sorted Array - LeetCode"
  url="https://leetcode.com/problems/merge-sorted-array/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-27" >}}

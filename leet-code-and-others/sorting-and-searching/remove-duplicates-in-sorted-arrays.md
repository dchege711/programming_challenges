---
date: 2026-05-28
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/remove-duplicates-in-sorted-arrays/
tags:
- two-pointer-algorithm
title: Remove Duplicates from Sorted Array
---

Given a sorted integer array, remove some duplicates in-place such that each
unique element appears at most twice. Maintain the relative order of the
elements. If there are \\(k\\) elements after removing the duplicates, the first
\\(k\\) elements of the array should hold the final result. {{% cite
LCRemoveDuplicatesSortedII %}}

This a two-pointer problem, where we have a tortoise and a hare. Establishing an
invariant that must be maintained throughout the array is useful to avoid
tripping up on index arithmetic. In this case, let `tortoise` be the index whose
final value needs to be determined in the current iteration. `[..., tortoise-1]`
(inclusive) is the processed range that won't change. {{% tag
two-pointer-algorithm %}}

```cs
public int RemoveDuplicates(int[] nums) {
  if (nums.Length == 0)
    return 0;

  int tortoise = 1, count = 1;
  for (int hare = 1; hare < nums.Length; hare++)
  {
    bool hareMatchesPrevious = nums[hare] == nums[hare-1];
    count = hareMatchesPrevious ? count + 1 : 1;

    if (hareMatchesPrevious && count <= 2)
    {
      nums[tortoise] = nums[hare];
      tortoise += 1;
    }
    else if (!hareMatchesPrevious)
    {
      nums[tortoise] = nums[hare];
      tortoise += 1;
      count = 1;
    }
  }
  return tortoise;
}
```

1. {{< citation
  id="LCRemoveDuplicatesSortedII"
  title="Remove Duplicates from Sorted Array II - LeetCode"
  url="https://leetcode.com/problems/remove-duplicates-from-sorted-array-ii/submissions/2015545400/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-28" >}}

---
date: 2026-06-06
domains:
- docs.python.org
- leetcode.com
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/overlapping-intervals/
tags:
- intervals-algorithms
title: Of [Non]Overlapping Intervals
---

{{% tag intervals-algorithms %}}

## Merge Overlapping Intervals

### Problem Statement: Merge Overlapping Intervals

Given an integer array `intervals` where `intervals[i] = [start_i, end_i]`,
merge all overlapping intervals and return an array of the non-overlapping
intervals. For example \\([[1,3],[2,6],[8,10],[15,18]] \to
[[1,6],[8,10],[15,18]]\\) because \\([1,3]\\) and \\([2,6]\\) overlap and merge
into \\([1,6]\\). {{% cite LCMergeIntervals %}}

### Solution: Sort by Start Time and then Linear Scan

Like [Minimum Size Subarray Sum]({{< ref
"/computer-science/programming-challenges/leet-code-and-others/strings-and-1d-arrays/min-size-subarray-sum"
>}}), contiguity hints at a linear scan being useful. If we sort `intervals` by
`start`, then it's a matter of starting at some `intervals[i]` and expanding
until some `intervals[j]`, \\(j > i\\) is no longer overlapping.

<details>
<summary>Implementation: Sort then Linear Scan</summary>

```py
def merge(self, intervals: List[List[int]]) -> List[List[int]]:
  intervals.sort(key= lambda x: x[0])
  i = 0
  merged_intervals: List[List[int]] = []
  while i < len(intervals):
    merged_interval = intervals[i]
    i += 1
    while i < len(intervals):
      if intervals[i][0] > merged_interval[1]:
        break

      merged_interval[1] = max(merged_interval[1], intervals[i][1])
      i += 1

    merged_intervals.append(merged_interval)

  return merged_intervals
```

</details>

{{% cite HelloInterviewIntervals %}} has a cleaner solution that avoids
mutation, index arithmetic, and nested loops.

<details>
<summary>Implementation: Sort then Linear Scan Without Index Arithmetic</summary>

```py
def merge(self, intervals: List[List[int]]) -> List[List[int]]:
  sorted_intervals = sorted(intervals, key=lambda x: x[0])
  merged_intervals = []

  for interval in sorted_intervals
    if not merged_intervals or interval[0] > merged_intervals[-1][1]:
      merged_intervals.append(interval)
    else:
      merged_intervals[-1][1] = max(merged_intervals[-1][1], interval[1])

  return merged_intervals
```

</details>

{{% comment %}}

Next time, be wary of nested loops that aren't really \\(\mathcal{O}(N^2)\\).
There's a reasonable chance that a single loop will do given that you're doing
\\(\mathcal{O}(N)\\) work anyway.

{{% /comment %}}

## Non-Overlapping Intervals

### Problem Statement: Non-Overlapping Intervals

Given an integer array `intervals` where `intervals[i] = (start_i, end_i)`,
return the minimum number of intervals you need to remove to make the rest of
the intervals non-overlapping. Note that \\((1, 2)\\) and \\((2, 3)\\) are
considered non-overlapping. {{% cite LCNonOverlappingIntervals %}}

### Solution: Sort by End Time and then Linear Scan

If minimizing the number of overlaps (and thus maximizing the number of
non-overlaps), then it helps to sort by `end_i`. Sorting by `start_i` risks
commiting to an interval that starts early and ends late, preventing us from
choosing multiple non-overlapping intervals in between. {{% cite
HelloInterviewIntervals %}}

<details>
<summary>Implementation: Sort by End Time and then Linear Scan</summary>

```py
from itertools import islice

def min_removals_for_non_overlap(self, intervals: List[List[int]]) -> int:
  if not intervals:
    return 0

  sorted_intervals = sorted(intervals, key=lambda x: x[1])

  non_overlapping_interval_end = sorted_intervals[0][1]
  overlapping_interval_count = 0

  for interval in islice(sorted_intervals, 1, len(sorted_intervals)):
    if non_overlapping_interval_end > interval[0]:
      overlapping_interval_count +=1
    else:
      non_overlapping_interval_end = interval[1]

  return overlapping_interval_count
```

</details>

{{% comment %}}

TIL about {{% cite itertools.islice %}}, which is a memory efficient way for
iterating through a subarray without creating a copy.

{{% /comment %}}

## References

1. {{< citation
  id="LCMergeIntervals"
  title="Merge Intervals - LeetCode"
  url="https://leetcode.com/problems/merge-intervals/submissions/2024822671/"
  accessed="2026-06-06" >}}

1. {{< citation
  id="HelloInterviewIntervals"
  title="Intervals Overview | Hello Interview"
  url="https://www.hellointerview.com/learn/code/intervals/overview"
  accessed="2026-06-06" >}}

1. {{< citation
  id="LCNonOverlappingIntervals"
  title="Non-overlapping Intervals - LeetCode"
  url="https://leetcode.com/problems/non-overlapping-intervals/description/"
  accessed="2026-06-06" >}}

1. {{< citation
  id="itertools.islice"
  title="itertools — Functions creating iterators for efficient looping — Python 3.14.5 documentation"
  url="https://docs.python.org/3/library/itertools.html#itertools.islice"
  url_2="https://docs.python.org/3/library/functions.html#slice"
  accessed="2026-06-06" >}}

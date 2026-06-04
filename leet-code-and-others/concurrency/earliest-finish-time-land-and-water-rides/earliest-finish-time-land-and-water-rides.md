---
date: 2026-06-03
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/concurrency/earliest-finish-time-land-and-water-rides/earliest-finish-time-land-and-water-rides/
title: Earliest Finish Time for Land and Water Rides
---

## Problem Statement

There are \\(L\\) possible land rides and \\(W\\) possible water rides, with
\\(1 \le L, W \le 5 \times 10^4\\). A tourist must experience exactly one ride
from each category, in either order. A ride may be started at its opening time
or any later moment. Immediately after finishing one ride, the tourist may board
the other if it's already open or wait until it opens. Return the earliest
possible time at which the tourist can finish both rides. {{% cite
LCEarliestFinishTime %}}

## Solution

That {{% cite LCEarliestFinishTime %}} is tagged concurrency makes me think that
I need some synchronization primitives for this. Having a hard time framing {{%
cite LCEarliestFinishTime %}} as a multi-threading problem though.

How can we solve this without synchronization primitives?

1. Initialize \\(t_{earliest} \leftarrow \infty\\)
2. For ride \\(l \in L\\), choose \\(l\\) such that \\(l_{start} <
   t_{earliest}\\).
   1. For each \\(w \in W\\), choose whether to take the ride before or
      after \\(l\\), preferring before whenever possible.
      1. Compute \\(t_{candidate} = max(l_{end}, w_{end})\\)
      2. Update \\(t_{earliest} \leftarrow min(t_{candidate}, t_{earliest})\\)

This algorithm would run in \\(\mathcal{O}(WN)\\), which is quadratic. Let's try
the \\(\mathcal{O}(WN)\\) solution and see how much perf is needed for {{% cite
LCEarliestFinishTime %}}. Welp, \\(\mathcal{O}(WN)\\) is too slow.

Can sorting help reduce the search space so that we run in less than
\\(\mathcal{O}(WN)\\) time?

<details>
<summary>Implementation: O(WN)</summary>

{{< readfile
   file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/earliest-finish-time-land-and-water-rides/quadratic_solution.py"
   highlight="py" >}}

</details>

## References

1. {{< citation
  id="LCEarliestFinishTime"
  title="Earliest Finish Time for Land and Water Rides II - LeetCode"
  url="https://leetcode.com/problems/earliest-finish-time-for-land-and-water-rides-ii/description/"
  accessed="2026-06-03" >}}

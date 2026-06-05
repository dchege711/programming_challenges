---
cited-authors:
- Singh, Bijoy
date: 2026-06-03
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/earliest-finish-time-land-and-water-rides/earliest-finish-time-land-and-water-rides/
tags:
- greedy-algorithm
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

When in doubt, brute-force:

1. Initialize \\(t_{earliest} \leftarrow \infty\\)
2. For ride \\(l \in L\\), choose \\(l\\) such that \\(l_{start} <
   t_{earliest}\\).
   1. For each \\(w \in W\\), choose whether to take the ride before or
      after \\(l\\), preferring before whenever possible.
      1. Compute \\(t_{candidate} = max(l_{end}, w_{end})\\)
      2. Update \\(t_{earliest} \leftarrow min(t_{candidate}, t_{earliest})\\)
3. Repeat steps (1) and (2) but in (2) start by choosing from \\(W\\) first, and
   then from \\(L\\).

This algorithm would run in \\(\mathcal{O}(LW)\\), which is quadratic. Let's try
the \\(\mathcal{O}(LW)\\) solution and see how much perf is needed for {{% cite
LCEarliestFinishTime %}}. Welp, \\(\mathcal{O}(LW)\\) is too slow; times out at
the 625th of 627 test cases.

<details>
<summary>Implementation: O(LW)</summary>

{{< readfile
   file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/earliest-finish-time-land-and-water-rides/quadratic_solution.py"
   highlight="py" >}}

</details>

Can sorting help reduce the search space so that we run in less than
\\(\mathcal{O}(LW)\\) time? Sorting \\(L\\) and \\(W\\) should helps us
disqualify more fixed rides faster, but that's still not fast enough for {{%
cite LCEarliestFinishTime %}}.

{{% cite LCEarliestFinishTimeSingh %}} describes an \\(\mathcal{O}(L + W)\\)
algorithm. Suppose we take a ride \\(l \in L\\) first. If \\(l\\) finishes at
\\(l_{finish} = l_{available} + l_{duration}\\), then a \\(w \in W\\) can only
start at \\(\max(l_{finish}, w_{available})\\), and finish at \\(w_{finish} =
\max(l_{finish}, w_{available}) + w_{duration}\\). \\(w_{finish}\\) is minimized
by minimizing \\(l_{finish}\\). The same reasoning applies when finishing a
water ride first. {{% tag greedy-algorithm %}}

My confusion came from "... a \\(w \in W\\) can only start at
\\(\max(l_{finish}, w_{available})\\)...". While it's possible to have a \\(w\\)
that can finish earlier than \\(l_{start}\\), that goes against "Suppose we take
a ride \\(l \in L\\) first..." Furthermore, any such \\(w\\) will be discovered
when we do the second pass where we choose a minimal \\(w_{finish}\\) before
looking at \\(l \in L\\).

<details>
<summary>Implementation: O(L + W)</summary>

{{< readfile
   file="/content/computer-science/programming-challenges/leet-code-and-others/concurrency/earliest-finish-time-land-and-water-rides/linear_solution.py"
   highlight="py" >}}

</details>

## References

1. {{< citation
  id="LCEarliestFinishTime"
  title="Earliest Finish Time for Land and Water Rides II"
  url="https://leetcode.com/problems/earliest-finish-time-for-land-and-water-rides-ii/description/"
  accessed="2026-06-03" >}}

1. {{< citation
  id="LCEarliestFinishTimeSingh"
  author="Bijoy Singh"
  title="Earliest Finish Time for Land and Water Rides II"
  sub-title="✅ Optimal O(n + m) Approach | Greedy Observation | C++ | Java | Python | JavaScript | Go"
  url="https://leetcode.com/problems/earliest-finish-time-for-land-and-water-rides-ii/solutions/8309992/optimal-on-m-approach-greedy-observation-ih2a/?envType=daily-question&envId=2026-06-03"
  accessed="2026-06-04" >}}

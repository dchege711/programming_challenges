---
date: 2024-07-21
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/interview-prep/strings/minimum-penalty-for-a-shop/
title: Minimum Penalty for a Shop
---

{{< citation
  id="leetCode2483"
  title="Minimum Penalty for a Shop"
  url="https://leetcode.com/problems/minimum-penalty-for-a-shop/description/"
  accessed="2024-07-21" >}}

## Problem

You are given the customer visit log of a shop represented by a
zero-indexed string `customers` consisting only of characters `N` and
`Y`. If the \\(i^{th}\\) character is `Y`, it means that customers come
at the \\(i^{th}\\) hour, whereas `N` indicates that no customers come
at the \\(i^{th}\\) hour.

If the shop closes at the \\(j^{th}\\) hour (\\(0 le j \le n\\)), the
penalty is calculated as follows:

* For every hour when the shop is open and no customers come, the
  penalty increases by \\(1\\).
* For every hour when the shop is closed and customers come, the penalty
  increases by \\(1\\).

Return the earliest hour at which the shop must be closed to incur a
minimum penalty. Note that if the shop closes at the \\(j^{th}\\) hour,
it means the shop is closed at the hour \\(j\\).

## Solution

Consider the string `YYNY`. There are two parts to the penalty at each
hour:

| Hour | Pre-Close Penalty | Post-Close Penalty | Penalty |
| --- | --- | --- | --- |
| 0 | 0 | 3 `YYNY` | 3 |
| 1 | 0 `Y` | 2 `YNY` | 2 |
| 2 | 0 `YY`| 1 `NY` | 1 |
| 3 | 1 `YYN` | 1 `Y` | 2 |
| 4 | 1 `YYNY` | 0 | 1 |

The pre-close and post-close penalties are both monotonic. At each step,
there is a penalty of \\(1\\) that either goes to either increasing the
pre-close penalty or decreasing the post-close penalty.

<details>
<summary>Implementation</summary>

```py
def best_closing_time(customers: str) -> int:
    # Set the initial values.
    suffix_penalty = sum(1 if c == 'Y' else 0 for c in customers)
    prefix_penalty = 0
    best_hour = 0
    min_penalty = suffix_penalty + prefix_penalty

    # Review the rest of the string and update the best value found.
    for hour in range(1, len(customers) + 1):
        if customers[hour-1] == 'Y':
            suffix_penalty -= 1
        else:
            prefix_penalty += 1

        penalty = prefix_penalty + suffix_penalty
        if penalty < min_penalty:
            best_hour = hour
            min_penalty = penalty

    return best_hour
```

Runtime \\(\mathcal{O}(N)\\). Space usage: \\(\mathcal{O}(1)\\).

</details>

I've seen this pattern before in [Minimizing Bottom-Right Paths in a 2xN
Grid]({{< ref
"/computer-science/programming-challenges/interview-prep/grids/grid_game">}}),
where we have a initial value that we update on seeing the next item in
the collection.

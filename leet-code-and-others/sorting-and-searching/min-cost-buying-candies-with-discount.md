---
date: 2026-06-01
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/min-cost-buying-candies-with-discount/
title: Minimum Cost of Buying Candies with Discount
---

The customer can choose any candy to takeaway for free as long as the cost of
the chosen candy is less than or equal to the minimum cost of the two candies
bought. Find the minimum cost of buying all the candies. {{% cite
LCBuyingCandiesWithDiscount %}}

<details>
<summary>Implementation: Sort then Traverse</summary>

```py
def minimumCost(self, cost: List[int]) -> int:
    cost.sort(reverse=True)
    minCost = 0
    for i in range(len(cost)):
        if i % 3 != 2:
            minCost += cost[i]

    return minCost
```

</details>

{{% comment %}}

Writing the above algorithm felt right, but I couldn't prove why it was optimal.

{{% /comment %}}

Why is sorting in descending order and then skipping every 3rd candy optimal?
A proof in two steps:

1. Maximizes the number of free candies. This max equals \\(\lfloor n/3
   \rfloor\\).
2. Among all purchase plans that obtain \\(\lfloor n/3 \rfloor\\) free candies,
   sorting in descending order and then skipping every 3rd candy optimal

For (1), any plan with less than \\(\lfloor n/3 \rfloor\\) free candies must
have at least 3 candies that have not been grouped together. Among these 3
candies, the cheapest one can be obtained for free by grouping them into a valid
purchase. {{% cite LCBuyingCandiesWithDiscount %}}

For (2), the \\(k\\)-th most expensive candy that can be obtained for free,
where \\(0 \le k \le \lfloor n/3 \rfloor\\), is at most `cost[3k + 2]`. {{% cite
LCBuyingCandiesWithDiscount %}}

1. {{< citation
  id="LCBuyingCandiesWithDiscount"
  title="Minimum Cost of Buying Candies With Discount"
  url="https://leetcode.com/problems/minimum-cost-of-buying-candies-with-discount/description/"
  accessed="2026-06-01" >}}

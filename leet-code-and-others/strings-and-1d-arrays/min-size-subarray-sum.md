---
date: 2026-05-28
domains:
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/strings-and-1d-arrays/min-size-subarray-sum/
title: Minimum Size Subarray Sum
---

Given an array of positive integers `nums` and a positive integer `target`,
return the minimal length of a subarray whose sum is greater than or equal to
`target`. Return `0` if there is no such subarray. {{% cite LCMinSizeSubarraySum
%}}

This reads like an \\(^{n}P_{k}\\) permutation problem, where we're trying to
minimize \\(k\\) under the constraint that the sum of elements is greater than
`target`. \\(^{n}P_{k}\\) can be implemented in a BFS manner, e.g.,

<details>
<summary>Implementation: BFS Approach</summary>

```cs
public class Solution {
    public int MinSubArrayLen(int target, int[] nums) {
        int numsTotal = nums.Sum();
        if (numsTotal < target)
            return 0;

        List<SubArray> candidates = [];
        for (var i = 0; i < nums.Length; i++)
        {
            if (nums[i] >= target)
                return 1;

            candidates.Add(new SubArray(new HashSet<int>([i]), nums[i]));
        }

        while (candidates.Any())
        {
            List<SubArray> nextCandidates = [];
            foreach (var subArray in candidates)
            {
                for (int i = 0; i < nums.Length; i++)
                {
                    if (subArray.Indices.Contains(i))
                        continue;

                    int newSum = subArray.Sum + nums[i];
                    if (newSum >= target)
                        return subArray.Indices.Count + 1;

                    var newIndices = new HashSet<int>(subArray.Indices);
                    newIndices.Add(i);
                    nextCandidates.Add(new(newIndices, newSum));
                }
            }
            candidates = nextCandidates;
        }

        return 0;
    }

    private record struct SubArray(IReadOnlySet<int> Indices, int Sum);
}
```

</details>

Ah, a subarray is defined to be a contiguous non-empty sequence of elements
within an array. {{% cite LCMinSizeSubarraySum %}} Confused it for a subsequence
which doesn't need to be contiguous, but on second thought, subsequences
maintain the relative order, while \\(^{n}P_{k}\\) does not preserve relative
order.

For subarrays, we can iterate over \\(k = 1, ..., n\\). The running sum is more
of a sliding window where elements fall off on the left and new ones get picked
up on the right, allowing for efficient computation.

<details>
<summary>Implementation: Sliding Windows of Sizes <code>k = 1, ..., n</code></summary>

```cs
public int MinSubArrayLen(int target, int[] nums)
{
    int totalSum = nums.Sum();
    if (totalSum < target)
        return 0;

    for (int k = 1; k <= nums.Length; k++)
    {
        int windowSum = 0;
        for (int i = 0; i < k; i++)
            windowSum += nums[i];

        if (windowSum >= target)
            return k;

        for (int i = k; i < nums.Length; i++)
        {
            windowSum += nums[i] - nums[i - k];
            if (windowSum >= target)
                return k;
        }
    }

    return 0;
}
```

</details>

The above implementation times out on an input where \\(N = 100,000\\) and
`target` is \\(396,893,380\\). What am I missing? Sorting is out of the question
because we must preserve `nums` original order.

Aha, we don't need to start from scratch by doing \\(k = 1, ..., n\\)
separately. If we have two pointers, `tortoise` and `hare`, we can consider
different values of \\(k\\) while adjusting `tortoise` and `hare` \\(2N\\)
times.

<details>
<summary>Implementation: Linear Scan Using Two Pointers</summary>

```cs
public class Solution
{
    public int MinSubArrayLen(int target, int[] nums)
    {
        int N = nums.Length;
        if (N == 0) return 0;

        int tortoise = 0, hare = 0, sum = nums[0], k = N + 1;
        while (tortoise < N && hare < N)
        {
            if (sum >= target)
            {
                k = Math.Min(k, hare - tortoise + 1);

                sum -= nums[tortoise];
                tortoise += 1;
            }
            else
            {
                hare += 1;
                if (hare < N) sum += nums[hare];
            }
        }

        return k > N ? 0 : k;
    }
}
```

</details>

Suppose we have \\([1, -1, 3]\\) and `target = 3`. We'd expand until \\([1, -1,
3]\\) with \\(k_{min} = 3\\). When shrinking the window, \\([-1, 3]\\) would
make us stop shrinking because \\(2 < 3\\). That all numbers in `nums` are
positive allows us to stop shrinking correctly because once `sum < target`, no
further left shrinking can give us `sum >= target`.

1. {{< citation
  id="LCMinSizeSubarraySum"
  title="Minimum Size Subarray Sum - LeetCode"
  url="https://leetcode.com/problems/minimum-size-subarray-sum/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-28" >}}

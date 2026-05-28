---
date: 2026-05-27
domains:
- en.wikipedia.org
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/sorting-and-searching/majority-element-in-array/
title: Majority Element in Array
---

Given an array of size \\(N\\), return the element that appears more than
\\(\lfloor N/2 \rfloor\\) times. Solve the problem in \\(\mathcal{O}(N)\\) time
and \\(\mathcal{O}(1)\\) space. {{% cite LCMajorityElement %}}

Solving this in \\(\mathcal{O}(N)\\) time precludes sorting as that takes
\\(\mathcal{O}(N\ logN)\\) time. Using \\(\mathcal{O}(1)\\) space precludes
having a dictionary of frequencies. What is special about a frequency that is
\\(> \lfloor N/2 \rfloor\\)? What kind of arithmetic would such a value
survive?

In a sorted array, a number that appears \\(> \lfloor N/2 \rfloor\\) times is
also the median. How can we compute the median of an unsorted array in
\\(\mathcal{O}(N)\\) time and \\(\mathcal{O}(1)\\) space? Scratch that, the
median is a more general problem as the frequency need not be \\(> \lfloor N/2
\rfloor\\).

Aha, a number that appears \\(> \lfloor N/2 \rfloor\\) times will survive a
knockout series involving \\(N - 1\\) comparisons, i.e.,

```cs
public int MajorityElement(int[] nums) {
    if (nums.Length == 0)
        throw new InvalidOperationException("No majority element found.");

    (int best, int strength) = (nums[0], 0);
    for (int i = 1; i < nums.Length; i++)
    {
        int candidate = nums[i];
        strength += candidate == best ? 1 : -1;

        if (strength < 0)
            (best, strength) = (candidate, 0);
    }

    if (strength < 0)
        throw new InvalidOperationException("No majority element found.");

    return best;
}
```

This algorithm is called the Boyer-Moore majority vote algorithm, and is a
prototypical example of a streaming algorithm:

```cs
public int MajorityElement(int[] nums) {
    (int? m, int c) = (null, 0);
    foreach (int x in nums)
    {
        if (c == 0)
            (m, c) = (x, 1);
        else
            c += m == x ? 1 : -1;
    }

    int actualCount = nums.Count(x => x == m);
    if (m is null || actualCount <= nums.Length / 2)
        throw new InvalidOperationException("No majority element found.");

    return m;
}
```

{{% cite WikiBoyerMooreMajorityVote %}}

After processing \\(n\\) input elements, the input sequence can be partitioned
into \\((n-c)/2\\) pairs of unequal elements, and \\(c\\) copies of \\(m\\) left
over. It is trivially true when \\(n = c = 0\\), and the invariant is maintained
every time an element \\(x\\) is added:

* If \\(x = m\\), increment \\(c\\), the number of copies of \\(m\\)
* If \\(x \ne m\\) and \\(c > 0\\), remove one of the copies of \\(m\\) from the
  left-over set.
* If \\(c = 0\\), set \\(m \leftarrow x\\) and add \\(x\\) to the previously
  empty set of copies of \\(m\\) and set \\(c \leftarrow 1\\).

In the end, no element \\(x \ne m\\) can have a majority because \\(x\\) can
equal at most one element of each unequal pair and none of the remaining \\(c\\)
copies of \\(m\\). Thus, if there is a majority element, it can only be \\(m\\).
{{% cite WikiBoyerMooreMajorityVote %}}

When the sequence has no majority, the Boyer-Moore majority vote algorithm
reports one of the sequence elements as its result. The second pass is needed as
it's impossible for a sublinear-space algorithm to determine whether there
exists a majority element in a single pass through the input. {{% cite
WikiBoyerMooreMajorityVote %}}

1. {{< citation
  id="LCMajorityElement"
  title="Majority Element - LeetCode"
  url="https://leetcode.com/problems/majority-element/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-27" >}}

1. {{< citation
  id="WikiBoyerMooreMajorityVote"
  title="Boyer–Moore majority vote algorithm - Wikipedia"
  url="https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm"
  accessed="2026-05-27" >}}

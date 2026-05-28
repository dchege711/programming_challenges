---
date: 2026-05-28
domains:
- learn.microsoft.com
- leetcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/leet-code-and-others/strings-and-1d-arrays/is-subsequence/
title: Is Subsequence
---

Given two lowercase strings, `s` and `t`, return `true` if `s` is a subsequence
of `t`, or `false` otherwise. A subsequence of a string is a string that is
formed from the original string by deleting some (can be none) of the characters
while maintaining the relative positions of the remaining characters. {{% cite
LCIsSubsequence %}}

My solution was:

```cs
public class Solution {
  private string _s;
  private string _t;

  public bool IsSubsequence(string s, string t) {
    _s = s;
    _t = t;
    return IsReachable(0, 0);
  }

  private bool IsReachable(int si, int ti)
  {
    if (si == _s.Length)
      return true;

    char c = _s[si];
    for (int i = ti; i < _t.Length; i++)
    {
      if (_t[i] == c)
        return IsReachable(si + 1, i + 1);
    }
    return false;
  }
}
```

... where `_s` and `_t` avoid unnecessary string copies in the recursion tree.

{{% comment %}}

C# supports local functions, e.g.,

```cs
public bool IsSubsequence(string s, string t) {
  return IsReachable(0, 0);

  bool IsReachable(int si, int ti) { ... }
}
```

They are efficient for writing nested functions that can only be called from the
context of another method. Local functions are defined at compile time. {{% cite
DotNetLocalFunctions %}}

{{% /comment %}}

{{% cite LCIsSubsequence6743977 %}}'s iterative solution is much cleaner. Trying
to implement it myself:

```cs
public bool IsSubsequence(string s, string t) {
  int si = 0, ti = 0;
  while (si < s.Length)
  {
    bool foundNextChar = false;
    for (int i = ti; i < t.Length; i++)
    {
      if (t[i] == s[si])
      {
        ti = i + 1;
        si += 1;
        foundNextChar = true;
        break;
      }
    }
    if (!foundNextChar)
      return false;
  }
  return si == s.Length;
}
```

... but I still have `si`, `ti`, and `i` lingering over. Why can I not come up
with the simpler algorithm from {{% cite LCIsSubsequence6743977 %}}?

```cs
public bool IsSubsequence(string s, string t) {
  int si = 0, ti = 0;
  while (si < s.Length && ti < t.Length)
  {
    if (s[si] == t[ti])
      si += 1;

    ti += 1;
  }
  return si == s.Length;
}
```

I think I over-anchored too early on complicating this problem. I was already
thinking that I need to consider every possible starting point of the
subsequence. However, when looking for subsequence `ac` in `a1b2a1c`:

| 0 | 1 | 2 | 3 | 4 | 5 | 6 |
| --- | --- | --- | --- | --- | --- | --- |
| a | 1 | b | 2 | a | 1 | c |

... there is no point in considering `a` at index \\(4\\) when we've already
satisfied `a` from index \\(0\\).

1. {{< citation
  id="LCIsSubsequence"
  title="Is Subsequence - LeetCode"
  url="https://leetcode.com/problems/is-subsequence/description/?envType=study-plan-v2&envId=top-interview-150"
  accessed="2026-05-28" >}}

1. {{< citation
  id="LCIsSubsequence6743977"
  title="Is Subsequence - LeetCode > Two Pointer Solution"
  url="https://leetcode.com/problems/is-subsequence/solutions/6743977/video-two-pointer-solution-by-niits-7igj"
  accessed="2026-05-28" >}}

1. {{< citation
  id="DotNetLocalFunctions"
  title="Local functions - C# | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/local-functions"
  accessed="2026-05-28" >}}

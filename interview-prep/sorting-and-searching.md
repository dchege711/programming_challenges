---
authors:
- Arjomandi, E.
- Mirzaian, A.
date: 2022-08-01
domains:
- en.cppreference.com
- leetcode.com
- www.cse.yorku.ca
local_url: http://localhost:1313/computer-science/programming-challenges/interview-prep/sorting-and-searching/
publications:
- Information Processing Letters
title: Sorting and Searching
---

## Order Statistics

### K-th Smallest Element in Sorted Matrix

> Given an \\(N \times N\\) matrix, where each of the rows and columns
> are sorted in ascending order, return the \\(k^{th}\\) smallest
> element in the matrix. The memory complexity must be better than
> \\(O(N^2)\\).

It's not guaranteed that `matrix[r][c] > matrix[r-1][c]`, so we can't
compute the row (and similarly, the column) in which the \\(k^{th}\\)
smallest element is in.

Using a priority queue that holds the \\(K\\) smallest elements does not
work because the memory usage is \\(O(K)\\), where \\(K\\) can be any
value in \\([1, N^2]\\). Furthermore, that doesn't take into account
that the rows and columns are already sorted.

The fact that the rows and columns are sorted is reminiscent of merge
sort which deals with partially sorted arrays. But the fact that the
\\(k^{th}\\) smallest element may be in the last row/column implies that
we need to process all rows/columns, and therefore potentially use
\\(O(N^2)\\) space, which is not allowed.

Maybe we do need to compute the row and column that contains the
\\(k^{th}\\) smallest element? Say we want \\(k = 8\\) for the matrix
below.

$$
\begin{bmatrix}
1 & 5 & 9 \\\\
10 & 11 & 13 \\\\
12 & 13 & 15
\end{bmatrix}
$$

\\([1, 5, 9] \\cup [10, 12]\\) can give us at most the 5th smallest
element. Also considering \\([11, 13] \cup [13]\\), we can get the \\(5
\+ 3 = 8\\) smallest element. Computing the 3rd smallest element in
\\([11, 13] \cup [13]\\) gives \\(13\\), which is the expected answer.
But does this technique always work? Well, it fails for \\(k = 5\\);
it'd wrongly compute \\(12\\) from \\([1, 5, 9] \\cup [10, 12]\\); the
5th smallest element is \\(11\\).

{{% comment %}}

How does this question have 422k accepted solutions out of 698k
submitted solutions? The numbers suggest that this is something that a
lot of people solve. What am I missing? It's not straightforward to
me...

{{% /comment %}}

Can this be modeled as a DFS problem? Starting at `matrix[0][0]`, we
move either right or down, preferring the smaller value first, and count
the elements visited so far. Once we have a path of length \\(k\\), we
store that as our best answer, and backtrack, trying to find another
path of length \\(k\\), but with a lower number. I think this should
work. But DFS needs to keep track of `visited`, and this can use space
up to \\(O(N^2)\\)... Maybe we can reason out which elements have been
visited given that we're only moving right/down, and always to the
smaller number first (and if there's a tie, to the right neighbor
first)? Maybe the return value of the DFS can be right/down, and if its
down, it's time to backtrack.

{{% comment %}}

Coming from Python, I'm quite surprised that C++'s
`std::numeric_limits<int>::infinity()` evaluates to `0`.

`std::numeric_limits<T>::has_infinity()` is `false` for `int`, and
"usually `true` just for `float`, `double` and `long double`. Only types
capable of representing positive infinity as a distinct special value
have a meaningful result for `infinity()`. {{% cite cppReferenceInfinity
%}}

{{% /comment %}}

DFS as described above doesn't work. While \\(1 \le k \le N^2\\), the
longest path from going down/right is \\(N + N - 1 = 2N - 1 \le N^2,
\ \ \forall N \ge 1\\).

{{% comment %}}

Only after writing up [a DFS
solution](https://leetcode.com/submissions/detail/763075069/) did the
inadequacy above come to me. I need to get better at noting when an
algorithm won't work.

{{% /comment %}}

What about modeling it as a BFS problem? At each BFS step, we can expand
the covered nodes. Unlike DFS, this technique can cover the whole grid.
What is the max space usage at a BFS round? In general, the max
expansion step occurs when a node is connected to all other nodes, and
that's \\(O(V)\\), which is \\(O(N^2)\\) in our case. However, in the
problem's matrix, each element has only two neighbors (right & down),
and so the \\(O(N^2)\\) wouldn't happen. In the max case, every element
popped from the BFS queue leads to two elements, so at most, we'd have
\\(2N^2 / 3\\) elements in the new BFS queue, and this is still
\\(O(N^2)\\). Hmm... Doesn't hurt to try? Ended up using a PQ, and the
solution's runtime is in the 38th percentile, and the memory efficiency
is in the 59th percentile.

{{% cite hiepitLCKthSmallestElemInSortedMatrix %}} has a MinPQ-based
solution that takes into account that the rows are sorted.

```cpp
for (int r = 0; r < min(n, k); ++r) {
  min_pq.push({matrix[r][0], r, 0});
}

int ans = std::numeric_limits<int>::max();
for (int i = 1; i <= k; ++i) {
  vector<int> top = min_pq.top(); min_pq.pop();
  ans = top[0];
  int r = top[1], c = top[2];
  if (int c_next = c + 1; c_next < n) {
    min_pq.push({matrix[r][next_c], r, next_c});
  }
}

return ans;
```

Surprisingly, the above solution is slower and takes more memory than
the BFS + MinPQ version. And more surprisingly, a straight up iteration
over the grid while keeping a MinPQ beats the BFS that tries to
judiciously add items to the PQ. Maybe this question's cost is dominated
by cache locality? After all, the run time is \\(\le\\) 100ms.

## References

1. {{< citation
  id="LCKthSmallestElemInSortedMatrix"
  title="Kth Smallest Element in a Sorted Matrix - LeetCode"
  url="https://leetcode.com/problems/kth-smallest-element-in-a-sorted-matrix/"
  url_2="https://leetcode.com/submissions/detail/763109621/"
  accessed="2022-08-01" >}}

1. {{< citation
  id="cppReferenceInfinity"
  title="std::numeric_limits<T>::infinity - cppreference.com"
  url="https://en.cppreference.com/w/cpp/types/numeric_limits/infinity"
  accessed="2022-08-02" >}}

1. {{< citation
  id="Mirzaian1984"
  authors="A. Mirzaian; E. Arjomandi"
  title="Selection in X + Y And Matrices with Sorted Rows and Columns"
  year="1984"
  publication="Information Processing Letters, Vol. 20, 1985, 13-17"
  url="http://www.cse.yorku.ca/~andy/pubs/X+Y.pdf"
  url_2="https://leetcode.com/problems/kth-smallest-element-in-a-sorted-matrix/discuss/85170/O(n)-from-paper.-Yes-O(rows)."
  accessed="2022-08-06" >}}

1. {{< citation
  id="hiepitLCKthSmallestElemInSortedMatrix"
  title="[C++/Java/Python] MaxHeap, MinHeap, Binary Search - Picture Explain - Clean & Concise - LeetCode Discuss"
  url="https://leetcode.com/problems/kth-smallest-element-in-a-sorted-matrix/discuss/1322101/C%2B%2BJavaPython-MaxHeap-MinHeap-Binary-Search-Picture-Explain-Clean-and-Concise"
  accessed="2022-08-06" >}}

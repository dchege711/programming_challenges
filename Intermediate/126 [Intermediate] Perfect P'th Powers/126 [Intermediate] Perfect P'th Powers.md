---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [126 (Intermediate) Perfect P'th Powers](https://www.reddit.com/r/dailyprogrammer/comments/1fcpnx/053013_challenge_126_intermediate_perfect_pth/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): Perfect P'th Powers
(#IntermediateIcon)
An integer X is a "perfect square power" if there is some integer Y such that Y2 = X. An integer X is a "perfect cube power" if there is some integer Y such that Y3 = X. We can extrapolate this where P is the power in question: an integer X is a "perfect p'th power" if there is some integer Y such that YP = X.

Your goal is to find the highest value of P for a given X such that for some unknown integer Y, YP should equal X. You can expect the given input integer X to be within the range of an unsigned 32-bit integer (0 to 4,294,967,295).

Special thanks to the ACM collegiate programming challenges group for giving me the initial idea here.

(http://uva.onlinejudge.org/index.php?option=onlinejudge&page=show_problem&problem=1563)
# Formal Inputs & Outputs
## Input Description
You will be given a single integer on a single line of text through standard console input. This integer will range from 0 to 4,294,967,295 (the limits of a 32-bit unsigned integer).

## Output Description
You must print out to standard console the highest value P that fits the above problem description's requirements.

# Sample Inputs & Outputs
## Sample Input
Note: These are all considered separate input examples.


```
17

1073741824

25
```
## Sample Output
Note: The string following the result are notes to help with understanding the example; it is NOT expected of you to write this out.


```
1 (17^1)

30 (2^30)

2 (5^2)
```

----
## **DISCLAIMER**
This prompt has been adapted from [126 [Intermediate] Perfect P'th Powers](https://www.reddit.com/r/dailyprogrammer/comments/1fcpnx/053013_challenge_126_intermediate_perfect_pth/
)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [126 (Hard) Not-So-Normal Triangle Search](https://www.reddit.com/r/dailyprogrammer/comments/1i65z6/071213_challenge_126_hard_notsonormal_triangle/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Hard): Not-So-Normal  Triangle Search
(#HardIcon)
A three-dimensional triangle can be defined with three points in 3D space: one for each corner. One can compute the surface-normal of this triangle by using the three points to compute the cross-product.

(http://en.wikipedia.org/wiki/Normal_(geometry))
(http://en.wikipedia.org/wiki/Cross_product)
You will be given a set of N points, such that N is greater than or equal to 3. Your goal is to find the maximum set of non-intersecting triangles that can be constructed with these N points (points may be shared between triangles) such that this set's average surface normal is as close to the given vector's direction as possible.

"Closeness" between the average surface normal and target vector is defined as minimizing for the smallest angle between the two (as computed through the dot-product ). Though each triangle has two surface normals (one for each of the two sides), we don't care about which one you choose: just make sure that when printing the results, you respect the right-hand rule for consistency. At minimum, this set must match the target vector with less than 10 degrees of difference.

(http://en.wikipedia.org/wiki/Dot_product)
(http://en.wikipedia.org/wiki/Right-hand_rule)
Original author: /u/nint22. This challenge is a little more math-heavy than usual, but don't worry: the math isn't hard, and Wikipedia has all the formulas you'll need. Triangle-triangle intersection will be the most tricky part!

(/u/nint22)
# Formal Inputs & Outputs
## Input Description
You will be given an integer N which represents the N-following lines, each being a 3D point in space. Each line has three Real-numbers that are space -delimited. The last line, which will be line N+1, is the target vector that you are trying to align-with: it is also represented as three space-delimited Real-numbers.

(https://en.wikipedia.org/wiki/Real_number)
## Output Description
Find the largest set of triangles whose average surface normals match the target vector direction within at minimum 10 degrees. Print the result as one triangle per line, where a triangle is defined as the three point indices used. If no set is found, print "No valid result found".

# Sample Inputs & Outputs
## Sample Input

```
5
0.6652 -0.1405 0.7143
0.2223 0.3001 0.7125
-0.9931 0.9613 0.0669
0.0665 0.6426 -0.4931
-0.1100 -0.3525 0.3548
0.577 -0.577 0.577
```
## Sample Output
The author is still working on a solution to generate some results with; first person to post good demo data gets a +1 gold medal! The following results are "bad"/"faked", and are only examples of "valid output format".


```
0 1 2
1 4 2
```

----
## **DISCLAIMER**
This prompt has been adapted from [126 [Hard] Not-So-Normal Triangle Search](https://www.reddit.com/r/dailyprogrammer/comments/1i65z6/071213_challenge_126_hard_notsonormal_triangle/
)

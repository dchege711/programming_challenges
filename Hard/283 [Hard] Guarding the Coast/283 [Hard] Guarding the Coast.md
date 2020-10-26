---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [283 (Hard) Guarding the Coast](https://www.reddit.com/r/dailyprogrammer/comments/5320ey/20160916_challenge_283_hard_guarding_the_coast/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Imagine you're in charge of the coast guard for your island nation, but you're on a budget. You have to minimize how many boats, helicopters and crew members to adequately cover the coast. Each group is responsible for a square area of coastline. 

It turns out this has a mathematical relationship to some interesting mathematics. In fractal geometry, the Minkowski–Bouligand Dimension, or box counting dimension, is a means of counting the fractal geometry of a set S in Euclidian space Rn. Less abstractly, imagine the set S laid out in an evenly space grid. The box counting dimension would be the minimum number of square tiles required to cover the set.

(https://en.wikipedia.org/wiki/Minkowski%E2%80%93Bouligand_dimension)
More realistically, when doing this counting you'll wind up with some partial tiles and have to overlap, and that's OK - overlapping boxes are fine, gaps in coastal coverage are not. What you want to do is to minimize the number of tiles overall. It's easy over estimate, it's another to minimize. 

# Input Description
You'll be given two things: a tile size N representing the side of the square, and an ASCII art map showing you the coastline to cover. 

Example:


```
2

*****
*   *
*   *
*   *
*****
```
# Output Description
Your program should emit the minimum number of tiles of that size needed to cover the boundary. 

From the above example:


```
8
```
# Challenge Input

```
4

                     **
                   *   **
                  *     *
                 **      *
                *        *
               **         *
              *            *
             *            *
              **        **
                *      *
              **        ***
             *             *
            *               *
          **                *
         *                   **
        **                     *
      **                        *
     *                        **
      *                     **
       *                 ***
        **              *
       *                 *
     **                   **
    *                 ****
     **         ******           
       *********
```

----
## **DISCLAIMER**
This prompt has been adapted from [283 [Hard] Guarding the Coast](https://www.reddit.com/r/dailyprogrammer/comments/5320ey/20160916_challenge_283_hard_guarding_the_coast/
)

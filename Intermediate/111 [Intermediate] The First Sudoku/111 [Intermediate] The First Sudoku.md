---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [111 (Intermediate) The First Sudoku](https://www.reddit.com/r/dailyprogrammer/comments/12qi97/1162012_challenge_111_intermediate_the_first/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Find the lexicographically first valid sudoku. A valid sudoku consists of a 9x9 grid of the numbers 1-9 such that no number appears twice in any row or column, or in any of the 9 major 3x3 sub-grids. Two sudokus can be compared to determine which is lexicographically first like this: append the rows for each of the two sudokus together. Find the first number where they differ. Whichever sudoku has a smaller number in that position is lexicographically first.

Here's the solution you should get:

[1, 2, 3, 4, 5, 6, 7, 8, 9]
[4, 5, 6, 7, 8, 9, 1, 2, 3]
[7, 8, 9, 1, 2, 3, 4, 5, 6]
[2, 1, 4, 3, 6, 5, 8, 9, 7]
[3, 6, 5, 8, 9, 7, 2, 1, 4]
[8, 9, 7, 2, 1, 4, 3, 6, 5]
[5, 3, 1, 6, 4, 2, 9, 7, 8]
[6, 4, 2, 9, 7, 8, 5, 3, 1]
[9, 7, 8, 5, 3, 1, 6, 4, 2] 


```
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```

```
[4, 5, 6, 7, 8, 9, 1, 2, 3]
```

```
[7, 8, 9, 1, 2, 3, 4, 5, 6]
```

```
[2, 1, 4, 3, 6, 5, 8, 9, 7]
```

```
[3, 6, 5, 8, 9, 7, 2, 1, 4]
```

```
[8, 9, 7, 2, 1, 4, 3, 6, 5]
```

```
[5, 3, 1, 6, 4, 2, 9, 7, 8]
```

```
[6, 4, 2, 9, 7, 8, 5, 3, 1]
```

```
[9, 7, 8, 5, 3, 1, 6, 4, 2]
```
If you want more of a challenge, find the lexicographically first valid 16x16 sudoku, or even larger.

Thanks to user Thomas1122 for suggesting this problem in /r/dailyprogrammer_ideas!

(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [111 [Intermediate] The First Sudoku](https://www.reddit.com/r/dailyprogrammer/comments/12qi97/1162012_challenge_111_intermediate_the_first/
)

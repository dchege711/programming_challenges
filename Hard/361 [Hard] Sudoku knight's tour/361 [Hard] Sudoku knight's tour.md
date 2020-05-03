---
draft: true
----

# [361 (Hard) Sudoku knight's tour](https://old.reddit.com/r/dailyprogrammer/comments/8ked11/20180518_challenge_361_hard_sudoku_knights_tour/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Today's challenge is an optimization problem. When this post is 7 days old, whoever has submitted the best answer will receive +1 gold medal flair.

# Quick description
Consider the 81 digits encountered along a (open) knight's tour on a completed Sudoku grid. Put these digits in order to form a 81-digit number. Make this number as large as possible.

(https://en.wikipedia.org/wiki/Knight%27s_tour)
(https://en.wikipedia.org/wiki/Sudoku)
# Details
Here's the suggested format for your submission. If you don't want to use this format, feel free to use whatever format you like, as long as it's not too hard to understand.

Submit two 9x9 grids. The first one must be a valid completed Sudoku. That is, every cell must be one of the digits 1 through 9 such that the same digit does not appear twice in any row or column, or in any of the 9 major 3x3 blocks.

The second grid must be a valid open 9x9 knight's tour. That is, the numbers 1 through 81 must each appear once in the grid, and any pair of consecutive numbers must be separated by a knight's move in chess, meaning they must either be separated by 2 columns and 1 row, or 1 column and 2 rows.

Your score is the 81-digit number determined by taking the sequence of digits in the first grid in the order of the cells in the second grid.

# Example solution

```
1 6 4 8 2 7 3 5 9
7 5 3 6 9 4 8 2 1
8 2 9 1 5 3 4 7 6
6 4 7 3 1 9 2 8 5
3 9 5 2 4 8 6 1 7
2 1 8 7 6 5 9 3 4
9 7 1 4 8 2 5 6 3
4 8 6 5 3 1 7 9 2
5 3 2 9 7 6 1 4 8

77 26 73 62 75 60 55 30 15
72 63 76 27 4 29 14 59 56
25 78 5 74 61 54 57 16 31
64 71 24 53 28 3 38 13 58
79 6 51 70 37 12 43 32 17
50 65 8 23 52 69 2 39 44
7 80 49 36 11 42 45 18 33
66 9 22 47 68 35 20 1 40
81 48 67 10 21 46 41 34 19
```
The score for this example solution is:


```
999999988988889776877677866145613414423212645653125633314527585614235247412312375
```
# Process details
You may of course use existing code and publications to create your solution, including libraries that solve Sudokus and knight's tour problems.

I'll resolve ties and issues using my best judgment, including potentially awarding whoever contributed most to the best solution, if my criterion would technically give it to someone else. If you're not satisfied with my decision, please just let me know and we can work it out.

You may submit solutions to me via PM if you don't want other people to use your solution. However you are highly encouraged to at least post your score here to inspire the competition, and very highly encouraged to post all your solutions and code once the 7 days is over.


----
## **DISCLAIMER**
This prompt has been adapted from [361 [Hard] Sudoku knight's tour](https://old.reddit.com/r/dailyprogrammer/comments/8ked11/20180518_challenge_361_hard_sudoku_knights_tour/)

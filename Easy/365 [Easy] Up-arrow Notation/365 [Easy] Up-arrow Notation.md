---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [365 (Easy) Up-arrow Notation](https://old.reddit.com/r/dailyprogrammer/comments/8xbxi9/20180709_challenge_365_easy_uparrow_notation/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
We were all taught addition, multiplication, and exponentiation in our early years of math.  You can view addition as repeated succession.  Similarly, you can view multiplication as repeated addition.  And finally, you can view exponentiation as repeated multiplication.  But why stop there?  Knuth's up-arrow notation takes this idea a step further.  The notation is used to represent repeated operations.

(https://en.wikipedia.org/wiki/Knuth%27s_up-arrow_notation)
In this notation a single ↑ operator corresponds to iterated multiplication.  For example:


```
↑
```

```
2 ↑ 4 = ?
= 2 * (2 * (2 * 2)) 
= 2^4
= 16
```
While two ↑ operators correspond to iterated exponentiation.  For example:


```
↑
```

```
2 ↑↑ 4 = ?
= 2 ↑ (2 ↑ (2 ↑ 2))
= 2^2^2^2
= 65536
```
Consider how you would evaluate three ↑ operators.  For example:


```
↑
```

```
2 ↑↑↑ 3 = ?
= 2 ↑↑ (2 ↑↑ 2)
= 2 ↑↑ (2 ↑ 2)
= 2 ↑↑ (2 ^ 2)
= 2 ↑↑ 4
= 2 ↑ (2 ↑ (2 ↑ 2))
= 2 ^ 2 ^ 2 ^ 2
= 65536
```
In today's challenge, we are given an expression in Kuth's up-arrow notation to evalute.


```
5 ↑↑↑↑ 5
7 ↑↑↑↑↑ 3
-1 ↑↑↑ 3
1 ↑ 0
1 ↑↑ 0
12 ↑↑↑↑↑↑↑↑↑↑↑ 25
```
# Credit
This challenge was suggested by user /u/wizao, many thanks! If you have a challeng idea please share it in /r/dailyprogrammer_ideas and there's a good chance we'll use it.

(/u/wizao)
(/r/dailyprogrammer_ideas)
# Extra Info
This YouTube video, The Balloon Puzzle - The REAL Answer Explained ("Only Geniuses Can Solve"), includes exponentiation, tetration, and up-arrow notation. Kind of fun, can you solve it? 

(https://www.youtube.com/watch?v=DNmWtINlkhw)

----
## **DISCLAIMER**
This prompt has been adapted from [365 [Easy] Up-arrow Notation](https://old.reddit.com/r/dailyprogrammer/comments/8xbxi9/20180709_challenge_365_easy_uparrow_notation/
)

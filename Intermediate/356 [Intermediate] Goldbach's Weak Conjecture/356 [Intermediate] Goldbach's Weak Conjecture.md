---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [356 (Intermediate) Goldbach's Weak Conjecture](https://www.reddit.com/r/dailyprogrammer/comments/8bh8dh/20180411_challenge_356_intermediate_goldbachs/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
According to Goldbach’s weak conjecture, every odd number greater than 5 can be expressed as the sum of three prime numbers. (A prime may be used more than once in the same sum.) This conjecture is called "weak" because if Goldbach's strong conjecture (concerning sums of two primes) is proven, it would be true. Computer searches have only reached as far as 1018 for the strong Goldbach conjecture, and not much further than that for the weak Goldbach conjecture. 

In 2012 and 2013, Peruvian mathematician Harald Helfgott released a pair of papers that were able to unconditionally prove the weak Goldbach conjecture.

Your task today is to write a program that applies Goldbach's weak conjecture to numbers and shows which 3 primes, added together, yield the result.

# Input Description
You'll be given a series of numbers, one per line. These are your odd numbers to target. Examples:


```
11
35
```
# Output Description
Your program should emit three prime numbers (remember, one may be used multiple times) to yield the target sum. Example:


```
11 = 3 + 3 + 5
35 = 19 + 13 + 3
```
# Challenge Input

```
111
17
199
287
53
```

----
## **DISCLAIMER**
This prompt has been adapted from [356 [Intermediate] Goldbach's Weak Conjecture](https://www.reddit.com/r/dailyprogrammer/comments/8bh8dh/20180411_challenge_356_intermediate_goldbachs/
)

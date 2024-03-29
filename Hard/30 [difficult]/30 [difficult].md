---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [30 (difficult)](https://www.reddit.com/r/dailyprogrammer/comments/reah4/3262012_challenge_30_difficult/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

The Fibonacci numbers are defined recursively as

f(0) = 0
f(1) = 1
f(n) = f(n-2) + f(n-1)

Find the last eight digits of f( 1018 ).

If you have some computer science and/or discrete math training, this is not very difficult, but if you don't it can be really tricky. You can't just write a for-loop to calculate Fibonacci numbers one by one (and you certainly can't simply implement the recursive definition directly). That for-loop would have to run a quintillion times, and by the time it's finished, the sun will probably have exploded. You have to be more clever than that.

I should add that the identity you need to solve this problem is on the Wikipedia page for Fibonacci numbers. Using that identity and another algorithm solves the problem instantly (my Python program gives the answer in less than 0.1 seconds).

(/r/dailyprogrammer_ideas)
Edit : Note the "Intermediate" challenge #30 has been changed. Thank you!


----
## **DISCLAIMER**
This prompt has been adapted from [30 [difficult]](https://www.reddit.com/r/dailyprogrammer/comments/reah4/3262012_challenge_30_difficult/
)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [53 (intermediate)](https://www.reddit.com/r/dailyprogrammer/comments/tpxqc/5162012_challenge_53_intermediate/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

A simple pseudo-random number generator looks like this: 


```
s(0) = 123456789
s(n) = (22695477 * s(n-1) + 12345) mod 1073741824
```
So each number is generated from the previous one.

Using this generator, generate 10 million numbers (i.e. s(0) through s(9,999,999)) and find the 1000 largest numbers in that list. What is the sum of those numbers?

Try to  make your solution as efficient as possible. 

(http://www.reddit.com/user/sim642)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [53 [intermediate]](https://www.reddit.com/r/dailyprogrammer/comments/tpxqc/5162012_challenge_53_intermediate/
)

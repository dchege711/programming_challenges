---
draft: true
----

# [250 (Intermediate) Self-descriptive numbers](https://www.reddit.com/r/dailyprogrammer/comments/41tdzy/20160120_challenge_250_intermediate/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
A descriptive number tells us how many digits we have depending on its index.

For a number with n digits in it, the most significant digit stands for the '0's and the least significant stands for (n - 1) digit.

As example the descriptive number of 101 is 120 meaning:

Today we are looking for numbers that describe themself:

In mathematics, a self-descriptive number is an integer m that in a given base b is b digits long in which each digit d at position n (the most significant digit being at position 0 and the least significant at position b - 1) counts how many instances of digit n are in m.

Source

(https://en.wikipedia.org/wiki/Self-descriptive_number)
As example we are looking for a 5 digit number that describes itself. This would be 21200:


```
21200
```
# Formal Inputs & Outputs
## Input description
We will search for self descriptive numbers in a range.
As input you will be given the number of digits for that range.

As example 3 will give us a range between 100 and 999


```
3
```

```
100
```

```
999
```
## Output description
Print out all the self descriptive numbers for that range like this:


```
1210
2020
```
Or when none is found (this is very much possible), you can write something like this:


```
No self-descriptive number found
```
## In and outs
Sample 1

In


```
3
```
Out


```
No self-descriptive number found
```
Sample 2

In


```
4
```
Out


```
1210
2020
```
Sample 3

In


```
5
```
Out


```
21200
```
## Challenge input

```
8
10
13
15
```
# Notes/Hints
When the number digits go beyond 10 you know the descriptive number will have trailing zero's.

You can watch this for a good solution if you get stuck

(https://www.youtube.com/watch?v=1GKfEDvhWdY)
# Bonus
You can easily do this by bruteforcing this, but from 10 or more digit's on, this will take ages.

The bonus challenge is to make it run for the large numbers under 50 ms, here you have my time for 15 digits


```
time
```

```
real    0m0.018s
user    0m0.001s
sys     0m0.004s
```
# Finally
Have a good challenge idea?

Consider submitting it to /r/dailyprogrammer_ideas

(/r/dailyprogrammer_ideas)
And special thanks to /u/Vacster for the idea.

(/u/Vacster)
EDIT

Thanks to /u/wboehme to point out some typos

(/u/wboehme)

----
## **DISCLAIMER**
This prompt has been adapted from [250 [Intermediate] Self-descriptive numbers](https://www.reddit.com/r/dailyprogrammer/comments/41tdzy/20160120_challenge_250_intermediate/
)

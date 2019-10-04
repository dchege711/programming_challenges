---

layout: writeup
title: Fermat's Little Theorem (Intermediate)
date: 2018-01-26
type: writeup

---

## Description

[Link to original challenge](https://www.reddit.com/r/dailyprogrammer/comments/7pmt9c/20180110_challenge_346_intermediate_fermats/)

Most introductionary implementations for testing the primality of a number have a time complexity of $ O(\sqrt{n}) $. For large numbers this is not a feasible strategy, for example testing a [400 digit number](https://en.wikipedia.org/wiki/Largest_known_prime_number). Fermat's Little Theorem states:

> If $p$ is a prime number, then for any integer $a$, the number $a^p − a$ is an integer multiple of $p$

This can also be stated as: $a^p \mod p = a$. If $n$ is not prime, then, in general, most of the numbers $a < n$ will not satisfy the above relation. This leads to the following algorithm for testing primality:

* Given a number $n$, pick a random number $a < n$ and compute the remainder of $a^n \mod n$.
* If the result is not equal to $a$, then $n$ is certainly not prime. If it is $a$, then chances are good that $n$ is prime.
* Now pick another random number $a$ and test it with the same method. If it also satisfies the equation, then we can be even more confident that $n$ is prime.

By trying more and more values of $a$, we can increase our confidence in the result. This algorithm is known as the Fermat test.

* If $n$ passes the test for some random choice of $a$, the chances are better than even that $n$ is prime.
* If $n$ passes the test for two random choices of $a$, the chances are better than 3 out of 4 that $n$ is prime.

By running the test with more and more randomly chosen values of a we can make the probability of error as small as we like. Create a program to do Fermat's test on a number, given a required certainty. Let the power of the modulo guide you.

### My Solution

```python
{% include_relative 346_intermediate_fermats_little_theorem/primality_test.py %}
```

### Bonus Challenge

There do exist numbers that fool the Fermat test: numbers $n$ that are not prime and yet have the property that $a^n$ is congruent to a modulo $n$ for all integers $a < n$. Such numbers are extremely rare, so the Fermat test is quite reliable in practice. Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them other than that they are extremely rare. There are $255$ Carmichael numbers below $100,000,000$.

There are variants of the Fermat test that cannot be fooled by these. Implement one.

### Futher reading

* [SICP 1.2.6 (Testing for Primality)](https://mitpress.mit.edu/sicp/chapter1/node17.html)
* [Wolfram MathWorld: Primality Testing](http://mathworld.wolfram.com/topics/PrimalityTesting.html)
* [Wiki Modular exponentiation](https://en.wikipedia.org/wiki/Modular_exponentiation)

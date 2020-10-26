---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [357 (Hard) Continued Fractions](https://www.reddit.com/r/dailyprogrammer/comments/8dqzyi/20180420_challenge_357_hard_continued_fractions/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
In mathematics, a continued fraction is an expression obtained through an iterative process of representing a number as the sum of its integer part and the reciprocal of another number, then writing this other number as the sum of its integer part and another reciprocal, and so on. 

A continued fraction is an expression of the form


```
1
    x + ----------
               1
        y + -------
                  1
            z + ----
                 ...
```
and so forth, where x, y, z, and such are real numbers, rational numbers, or complex numbers. Using Gauss notation, this may be abbreviated as 


```
[x; y, z, ...]
```
To convert a continued fraction to an ordinary fraction, we just simplify from the right side, which may be an improper fraction, one where the numerator is larger than the denominator. 

Continued fractions can be decomposed as well, which breaks it down from an improper fraction to its Gauss notation. For example:


```
16        1
-- = 0 + ---
45        45
          --
          16
```
We can then begin to decompose this:


```
1
0 + ----------------
              1
    2 + ------------
              1
        1 + --------
                1
            4 + -
                3
```
So the Gauss notation would be [0;2,1,4,3]. 

Your challenge today is to implement a program that can do two things in the realm of continued fractions:

1) Given a Gauss representation of a continued fraction, calculate the improper fraction.
2) Given an improper fraction, calculate the Gauss representation. 

# Challenge Inputs

```
45
--
16


[2;1,7]

7
-
3
```
# Challenge Outputs

```
45
-- = [2;1,4,3]
16


          22
[2;1,7] = --
           7


7
- = [2;2,1,1]
3
```
# Bonus
Display the continued fraction. Mega bonus if you use MathML or LaTeX. 

(https://www.w3.org/Math/)
(https://www.latex-project.org/)
# Notes
https://en.wikipedia.org/wiki/Continued_fraction

(https://en.wikipedia.org/wiki/Continued_fraction)
http://www.cemc.uwaterloo.ca/events/mathcircles/2016-17/Fall/Junior78_Oct11_Soln.pdf

(http://www.cemc.uwaterloo.ca/events/mathcircles/2016-17/Fall/Junior78_Oct11_Soln.pdf)

----
## **DISCLAIMER**
This prompt has been adapted from [357 [Hard] Continued Fractions](https://www.reddit.com/r/dailyprogrammer/comments/8dqzyi/20180420_challenge_357_hard_continued_fractions/
)

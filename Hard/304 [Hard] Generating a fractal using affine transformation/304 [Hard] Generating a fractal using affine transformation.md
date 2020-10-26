---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [304 (Hard) Generating a fractal using affine transformation](https://www.reddit.com/r/dailyprogrammer/comments/5xaoxb/20170303_challenge_304_hard_generating_a_fractal/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
IFS (Iterated Function System) is a method of constructing fractals. To generate a fractal, we take a starting point (usually (1, 1)), and then transform it using equations in the form of:

(https://en.wikipedia.org/wiki/Iterated_function_system)

```
(1, 1)
```

```
a b c d e f
```
Transformation of a point with coordinates (x, y) gives us another point:


```
(x, y)
```

```
(ax+by+e, cx+dy+f)
```
We mark it on a plot and repeat the operation until we get a satisfying result.

A more popular way of generating fractals with IFS is so called Random IFS. The fractal is generated in the exact same way, except that we choose an equation from a set at random.

For example, the following set:


```
-0.4 0.0 0.0 -0.4 -1.0 0.1
0.76 -0.4 0.4 0.76 0.0 0.0
```
Results in a Heighway Dragon.

(http://i.imgur.com/8SIUUP3.png)
It turns out that by weighing the probabilities, we can alter the shape of the fractal and make it achieve its proper form faster. The probability of choosing an equation is denoted by an extra parameter p. For example:


```
p
```

```
0.0 0.0 0.0 0.16 0.0 0.0 0.01
0.2 -0.26 0.23 0.22 0.0 1.6 0.07
-0.15 0.28 0.26 0.24 0.0 0.44 0.07
0.85 0.04 -0.04 0.85 0.0 1.6 0.85
```
Is a set for Barnsley fern. Without the probability parameters, it doesn't look so great anymore (if p parameters are ommited, we assume uniform distribution of equations).

(http://i.imgur.com/8fvYvr1.png)
(http://i.imgur.com/wny7bnB.png)

```
p
```
Challenge: write your own fractal-generating program.

# Input
Minimal input will consist of a set of IFS equations. Other things to consider:

Size

"Density" of a fractal (how many pixels are generated) 

Aspect ratio of the image

# Output
An image of the resulting fractal.

# Sample input

```
0.000 0.000 0.000 0.600 0.00 -0.065 0.1
0.440 0.000 0.000 0.550 0.00 0.200 0.18
0.343 -0.248 0.199 0.429 -0.03 0.100 0.18
0.343 0.248 -0.199 0.429 0.03 0.100 0.18
0.280 -0.350 0.280 0.350 -0.05 0.000 0.18
0.280 0.350 -0.280 0.350 0.05 0.000 0.18
```
# Sample output

```
http://i.imgur.com/buwsrYY.png
```
More challenge inputs can can be found here and here

(http://cs.lmu.edu/%7Eray/notes/ifs/)
(http://paulbourke.net/fractals/)
# Credit
This challenge was suggested by /u/szerlok, many thanks! If you have any challenge ideas please share them on /r/dailyprogrammer_ideas and there's a good chance we'll use them. 

(/u/szerlok)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [304 [Hard] Generating a fractal using affine transformation](https://www.reddit.com/r/dailyprogrammer/comments/5xaoxb/20170303_challenge_304_hard_generating_a_fractal/
)

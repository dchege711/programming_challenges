---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [175 (Hard) Hall of Mirror()](https://www.reddit.com/r/dailyprogrammer/comments/2dmdwo/8152014_challenge_175_hard_hall_of_mirror/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Hard): Hall of Mirror[]
(#HardIcon)

```
Mirror[]
```
Today we're going to embark on some advanced geometry. You'll want to freshen up your angles and vectors because there will be a lot of them today!

We're going to be simulating the path of a light ray in 2D space through a hall of mirrors - a mirror being a plane of finite length that, upon the light ray hitting it, will reflect the light ray with the same angle of incidence like this image here. The mirrors are double-sided and have zero thickness.

(http://i.imgur.com/NcJrpRT.png)
You will be given a set of mirrors, defined by a start and end point, and a light ray, represented by a starting position, a starting vector (that may or may not be normalized) and a distance. You will have to simulate the light ray travelling for the given distance accounting for any reflections on the mirrors, assuming Euclidan geometry and no fancy stuff like refraction, special relativity or similar.

# Formal Inputs and Outputs
## Input Description
You will be given a number N, which is the number of mirrors in the world. You will then be given N lines of input in the format:


```
X1 Y1 X2 Y2
```
Where (X1,Y1) and (X2,Y2) represent the start and end points of a mirror.

After that you will be given one last line of input in the format:


```
PX PY VX VY D
```
Where (PX,PY) represents the starting position of the light ray in the world, (VX,VY) is the vector representing the light ray's direction in the world (be sure to normalize this beforehand) and D is the distance it will travel.

## Output Description
You will print a line in the format:


```
PX PY
```
Where (PX,PY) is the final position of the vector in the world.

# Sample Inputs & Output
## Sample Input

```
1
-1 0 1 0
-1 -1 1 1 2.828427
```
## Sample Output

```
1 -1
```
# Notes
You will need to have knowledge of the following things to solve this challenge:


----
## **DISCLAIMER**
This prompt has been adapted from [175 [Hard] Hall of Mirror[]](https://www.reddit.com/r/dailyprogrammer/comments/2dmdwo/8152014_challenge_175_hard_hall_of_mirror/
)

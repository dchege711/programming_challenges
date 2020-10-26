---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [321 (Hard) Circle Splitter](https://www.reddit.com/r/dailyprogrammer/comments/6ksmh5/20170630_challenge_321_hard_circle_splitter/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Hard): Circle Splitter
(#HardIcon)
(sorry for submitting this so late! currently away from home and apparently the internet hasn't arrived in a lot of places in Wales yet.)

Imagine you've got a square in 2D space, with axis values between 0 and 1, like this diagram. The challenge today is conceptually simple: can you place a circle within the square such that exactly half of the points in the square lie within the circle and half lie outside the circle, like here? You're going to write a program which does this - but you also need to find the smallest circle which solves the challenge, ie. has the minimum area of any circle containing exactly half the points in the square.

![Image_1](http://i.imgur.com/5K0HZEk.png)
![Image_2](http://i.imgur.com/n7BDeyg.png)

This is a hard challenge so we have a few constraints:

There are some inputs which cannot be solved. If there is no solution to this challenge then your solver must indicate this - for example, in this scenaro, there's no "dividing sphere" which lies entirely within the square.

![Image_3](http://i.imgur.com/fDGPvX3.png)
# Input & Output Description
## Input
On the first line, enter a number N. Then enter N further lines of the format x y which is the (x, y) coordinate of one point in the square. Both x and y should be between 0 and 1 inclusive. This describes a set of N points within the square. The coordinate space is R2 (ie. x and y need not be whole numbers).


```
x y
```
As mentioned previously, N should be an even number of points.

## Output
Output the centre of the circle (x, y) and the radius r, in the format:


```
x y
r
```
If there's no solution, just output:


```
No solution
```
# Challenge Data
There's a number of valid solutions for these challenges so I've written an input generator and visualiser in lieu of a comprehensive solution list, which can be found here. This can visualuse inputs and outputs, and also generate inputs. It can tell you whether a solution contains exactly half of the points or not, but it can't tell you whether it's the smallest possible solution - that's up to you guys to work out between yourselves. ;)

(https://jsfiddle.net/gjkdc8hL/)
## Input 1

```
4
0.4 0.5
0.6 0.5
0.5 0.3
0.5 0.7
```
## Potential Output

```
0.5 0.5
0.1
```
## Input 2

```
4
0.1 0.1
0.1 0.9
0.9 0.1
0.9 0.9
```
This has no valid solutions.

Due to the nature of the challenge, and the mod team being very busy right now, we can't handcraft challenge inputs for you - but do make use of the generator and visualiser provided above to validate your own solution. And, as always, validate each other's solutions in the DailyProgrammer community.

# Bonus
# We need more moderators!
We're all pretty busy with real life right now and could do with some assistance writing quality challenges. Check out jnazario's post for more information if you're interested in joining the team.

(https://www.reddit.com/r/dailyprogrammer/comments/6fm3yy/announce_seeking_moderators/)

----
## **DISCLAIMER**
This prompt has been adapted from [321 [Hard] Circle Splitter](https://www.reddit.com/r/dailyprogrammer/comments/6ksmh5/20170630_challenge_321_hard_circle_splitter/
)

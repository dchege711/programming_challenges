# [223 (Hard) The Heighway dragon fractal](https://www.reddit.com/r/dailyprogrammer/comments/3dl9wr/20150717_challenge_223_hard_the_heighway_dragon/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Write a program to print out the (x, y) coordinates of each point in the nth iteration of the Heighway dragon fractal. Start at the origin (0, 0) and take steps of length 1, starting in the positive x direction (1, 0), then turning to the positive y direction (1, 1). Your program should generate 2n + 1 lines of output.

(http://www-user.uni-bremen.de/schmuhl/fractals/dragon_curve_o12.png)
You can use any resources you want for help coming up with the algorithm, but if you want to start from the very beginning, use only the fact that the nth iteration can be made by folding a strip of paper in half n times, then unfolding it so that each crease is at a right angle.

(http://www.cutoutfoldup.com/images/0216-s03b.jpg)
# Example
For n = 3, your output should be:


```
0 0
1 0
1 1
0 1
0 2
-1 2
-1 1
-2 1
-2 2
```
Plotted image of these points, made using LibreOffice.

(http://i.imgur.com/3sCzNyG.png)
The sum of the x's here is -4, and the sum of the y's is 10. For n = 12, the sums are -104896 and 52416. To verify that your program is correct, post the sum of x's and y's for n = 16 along with your code.

# Optional challenges
Today's basic challenge is not too hard, relatively speaking, so if you want more, try some of these optional add-ons, or take it in your own direction.

(http://i.imgur.com/n30yp.gif)
(http://ecademy.agnesscott.edu/%7Elriddle/ifs/heighway/heighway.htm)

----
## **DISCLAIMER**
This prompt has been adapted from [223 [Hard] The Heighway dragon fractal](https://www.reddit.com/r/dailyprogrammer/comments/3dl9wr/20150717_challenge_223_hard_the_heighway_dragon/
)

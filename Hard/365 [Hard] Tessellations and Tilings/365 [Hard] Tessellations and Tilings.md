---
draft: true
----

# [365 (Hard) Tessellations and Tilings](https://old.reddit.com/r/dailyprogrammer/comments/8ylltu/20180713_challenge_365_hard_tessellations_and/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
A Tessellation (or Tiling) is the act of covering a surface with a pattern of flat shapes so that there are no overlaps or gaps. Tessellations express fascinating geometric and symmetric properties as art, and famously appear in Islamic art with four, five, and six-fold regular tessellations.

Today we'll your challenge is to write a program that can do basic regular tessellations in ASCII art. 

# Input Description
You'll be given an integer on the first line, which can be positive or negative. It tells you the rotation (relative to clockwise, so 180, 90, 0, or -90) to spin the tile as you tessellate it. The next line contains a single integer that tells your program how many columns and rows to read (assume it's a square). Then the next N rows contain the pattern of the tile in ASCII art. 

Example:


```
90
4
####
#--#
#++#
####
```
# Output Description
Your program should emit a tessellation of the tile, with the rotation rules applied, repeated at least two times in both the horizontal and vertical directions, you can do more if you wish. For the above:


```
########
#--##+|#
#++##+|#
########
########
#+|##++#
#+|##--#
########
```
# Challenge Input

```
90
6
/\-/|-
/\/-\/
||\\-\
|\|-|/
|-\|/|
|\-/-\
```

```
180
6
&`{!#;
#*#@+#
~/}}?|
'|(==]
\^)~=*
|?|*<%
```
# Bonus
Feel free to come up with some fun designs you can feed your program.

Feel free, also, to do this not with ASCII art but ANSI or even graphics. 


----
## **DISCLAIMER**
This prompt has been adapted from [365 [Hard] Tessellations and Tilings](https://old.reddit.com/r/dailyprogrammer/comments/8ylltu/20180713_challenge_365_hard_tessellations_and/
)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [166 (Easy) ASCII Fractal Curves](https://www.reddit.com/r/dailyprogrammer/comments/27pgqv/692014_challenge_166_easy_ascii_fractal_curves/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Easy): ASCII Fractal Curves
(#EasyIcon)
Today we're going to set a more open-ended challenge. First, let's look at what a space-filling curve is.

A space-filling curve is a specific type of line/curve that, as you recreate it in more and more detail, fills more and more of the space that it's in, without (usually) ever intersecting itself. There are several types of space-filling curve, and all behave slightly differently. Some get more and more complex over time whereas some are the same pattern repeated over and over again.

(http://en.wikipedia.org/wiki/Space-filling_curve)
Your challenge will be to take any space-fulling curve you want, and write a program that displays it to a given degree of complexity.

# Formal Inputs and Outputs
## Input Description
The input to this challenge is extremely simple. You will take a number N which will be the degree of complexity to which you will display your fractal curve. For example, this image shows the Hilbert curve shown to 1 through 6 degrees of complexity.

(http://upload.wikimedia.org/wikipedia/en/a/a5/Hilbert_curve.svg)
## Output Description
You must print out your own curve to the given degree of complexity. The method of display is up to you, but try and stick with the ASCII theme - for example, see below.

# Sample Inputs & Output
## Sample Input
(Hilbert curve program)


```
3
```
## Sample Output

```
# ##### ##### #
# #   # #   # #
### ### ### ###
    #     #    
### ### ### ###
# #   # #   # #
# ##### ##### #
#             #
### ### ### ###
  # #     # #  
### ### ### ###
#     # #     #
# ### # # ### #
# # # # # # # #
### ### ### ###
```
# Notes
Recursive algorithms will come in very handy here. You'll need to do some of your own research into the curve of your choice.


----
## **DISCLAIMER**
This prompt has been adapted from [166 [Easy] ASCII Fractal Curves](https://www.reddit.com/r/dailyprogrammer/comments/27pgqv/692014_challenge_166_easy_ascii_fractal_curves/
)

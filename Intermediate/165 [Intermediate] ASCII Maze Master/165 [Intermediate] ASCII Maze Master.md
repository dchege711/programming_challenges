---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [165 (Intermediate) ASCII Maze Master](https://www.reddit.com/r/dailyprogrammer/comments/278ptv/642014_challenge_165_intermediate_ascii_maze/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): ASCII Maze Master
(#IntermediateIcon)
We're going to have a slightly more logical puzzle today. We're going to write a program that will find a path through a simple maze.

A simple maze in this context is a maze where all of the walls are connected to each other. Take this example maze segment.


```
# # ### #
# #      
# ### B #
#   # B #
# B # B #
# B   B #
# BBBBB #
#       #
#########
```
See how the wall drawn with Bs isn't connected to any other walls? That's called a floating wall. A simple maze contains no floating walls - ie. there are no loops in the maze.


```
B
```
# Formal Inputs and Outputs
## Input Description
You will be given two numbers X and Y. After that you will be given a textual ASCII grid, X wide and Y tall, of walls # and spaces. In the maze there will be exactly one letter S and exactly one letter E. There will be no spaces leading to the outside of the maze - ie. it will be fully walled in.


```
#
```

```
S
```

```
E
```
## Output Description
You must print out the maze. Within the maze there should be a path drawn with askerisks * leading from the letter S to the letter E. Try to minimise the length of the path if possible - don't just fill all of the spaces with *!


```
*
```

```
S
```

```
E
```

```
*
```
# Sample Inputs & Output
## Sample Input

```
15 15
###############
#S        #   #
### ### ### # #
#   #   #   # #
# ##### ##### #
#     #   #   #
# ### # ### ###
# #   # #   # #
# # ### # ### #
# # # # # #   #
### # # # # # #
#   #   # # # #
# ####### # # #
#           #E#
###############
```
## Sample Output

```
###############
#S**      #   #
###*### ### # #
#***#   #   # #
#*##### ##### #
#*****#   #   #
# ###*# ### ###
# #***# #   # #
# #*### # ### #
# #*# # # #***#
###*# # # #*#*#
#***#   # #*#*#
#*####### #*#*#
#***********#E#
###############
```
# Challenge
## Challenge Input

```
41 41
#########################################
#   #       #     #           #         #
# # # ### # # ### # ####### ### ####### #
# #S#   # #   #   # #     #           # #
# ##### # ######### # # ############# # #
# #     # #         # #       #   #   # #
# # ##### # ######### ##### # # # # ### #
# #     #   #     #     #   # # # # # # #
# ##### ######### # ##### ### # # # # # #
#   #           #   #     #   # # #   # #
# ### ######### # ### ##### ### # ##### #
#   #   #     # # #   #       # #       #
# # ### # ### # ### ### ####### ####### #
# #     # #   #     #   # #     #     # #
# ####### # ########### # # ##### # ### #
#     # # #   #       #   # #   # #     #
##### # ##### # ##### ### # ### # #######
#   # #     # #   #   #   # #   #     # #
# ### ### ### ### # ### ### # ####### # #
#   #     #   #   # #   #   # #     #   #
### ##### # ### ### ### # ### # ### ### #
#       # #   # # #   # # #   # # #     #
# ####### ### # # ### ### # ### # #######
#       #   #   #   # #   #     #       #
# ##### ### ##### # # # ##### ### ### ###
#   # # #   #     # # #     # #     #   #
### # # # ### # ##### # ### # # ####### #
# #   #   #   # #     #   # # # #     # #
# ### ##### ### # ##### ### # # # ### # #
#   #       #   # # #   #   # # #   #   #
# # ######### ### # # ### ### # ### #####
# #     #   # # # #   #   # # #   #     #
# ##### # # # # # ### # ### # ######### #
# #   # # # # # #   # #   #             #
# # # # # # # # ### ### # ############# #
# # #     # # #   #   # #       #       #
# ######### # # # ### ### ##### # #######
#     #     # # #   #   # #     # #     #
# ### ####### ### # ### ### ##### # ### #
#   #             #   #     #       #E  #
#########################################
```
# Notes
One easy way to solve simple mazes is to always follow the wall to your left or right. You will eventually arrive at the end.


----
## **DISCLAIMER**
This prompt has been adapted from [165 [Intermediate] ASCII Maze Master](https://www.reddit.com/r/dailyprogrammer/comments/278ptv/642014_challenge_165_intermediate_ascii_maze/
)

# [338 (Intermediate) Maze turner](https://www.reddit.com/r/dailyprogrammer/comments/7aae56/20171102_challenge_338_intermediate_maze_turner/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Our maze explorer has some wierd rules for finding the exit and we are going to tell him if it is possible with his rules to get out.

Our explorer has the following rules:

# Formal Inputs & Outputs
## Input description
A maze with our explorer and the exit to reach

Legend: 


```
> : Explorer looking East
< : Explorer looking West
^ : Explorer looking North
v : Explorer looking south
E : Exit
# : wall
  : Clear passage way (empty space)
```
### Maze 1

```
#######
#>   E#
#######
```
### Maze 2

```
#####E#
#<    #
#######
```
### Maze 3

```
##########
#>      E#
##########
```
### Maze 4

```
#####E#
##### #
#>    #
##### #
#######
```
### Maze 5

```
#####E#
##### #
##### #
##### #
##### #
#>    #
##### #
#######
```
### Challenge Maze

```
#########
#E ######
##      #
##### # #
#>    ###
##### ###
##### ###
##### ###
##### ###
##### ###
##### ###
#########
```
### Challenge Maze 2

```
#########
#E ######
##      #
## ## # #
##### # #
#>    ###
##### ###
##### ###
##### ###
##### ###
##### ###
##### ###
#########
```
## Output description
Whetter it is possible to exit the maze 

### Maze 1

```
possible/true/yay
```
### Maze 2

```
possible/true/yay
```
### Maze 3

```
impossible/not true/darn it
```
### Maze 4

```
possible/true/yay
```
### Maze 5

```
impossible/not true/darn it
```
# Notes/Hints
Making a turn does not count as a step

# Several bonuses
## Bonus 1:
Generate your own (possible) maze.

## Bonus 2:
Animate it and make a nice gif out off it.

## Bonus 3:
Be the little voice in the head:

Instead of turning each 6 steps, you should implement a way to not turn if that would means that you can make it to the exit.

# Finally
Have a good challenge idea?

Consider submitting it to /r/dailyprogrammer_ideas

(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [338 [Intermediate] Maze turner](https://www.reddit.com/r/dailyprogrammer/comments/7aae56/20171102_challenge_338_intermediate_maze_turner/
)

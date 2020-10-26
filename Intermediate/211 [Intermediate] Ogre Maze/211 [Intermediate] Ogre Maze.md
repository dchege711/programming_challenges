---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [211 (Intermediate) Ogre Maze](https://www.reddit.com/r/dailyprogrammer/comments/33hwwf/20150422_challenge_211_intermediate_ogre_maze/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description:
Today we are going to solve a maze. What? Again? Come on, Simpsons did it. Yah okay so we always pick a hero to walk a maze. This time our hero is an Ogre.

An ogre is large. Your run of the mill hero "@" takes up a 1x1 spot. Easy. But our beloved hero today is an ogre.


```
@@
 @@
```
Ogres take up a 2x2 space instead of a 1x1. This makes navigating a maze tougher as you have to handle the bigger ogre.

So I will give you a layout of a swamp. (Ogres navigate swamps while puny heroes navigate caves. That's the unwritten rules of maze challenges) You will find the path (if possible) for
the ogre to walk to his gold. 

# Input:
You will read in a swamp. The swamp is laid out in 10x10 spaces. Each space can be the following:

# Output:
You will navigate the swamp. If you find a path you will display the solution of all the spaces the ogre will occupy to get to his gold. Use a "&" symbol to show the muddy path created by the ogre to reach his gold. If there is no path at all then you will output "No Path"

# Example Input 1:

```
@@........
 @@O.......
 .....O.O..
 ..........
 ..O.O.....
 ..O....O.O
 .O........
 ..........
 .....OO...
 .........$
```
# Example  Output 1:

```
&&.&&&&&&&
&&O&&&&&&&
&&&&&O.O&&
&&&&&&&&&&
..O.O&&&&&
..O..&&O.O
.O...&&&&.
.....&&&&.
.....OO&&&
.......&&&
```
# Example Input 2:

```
@@........
@@O.......
.....O.O..
..........
..O.O.....
..O....O.O
.O........
..........
.....OO.O.
.........$
```
# Example Output 2:
No Path

# FAQ (Will update with answers here)
### -
### -
(/r/dailyprogrammer)
# Challenge Input 1:

```
$.O...O...
...O......
..........
O..O..O...
..........
O..O..O...
..........
......OO..
O..O....@@
........@@
```
# Challenge Input 2:

```
.@@.....O.
.@@.......
..O..O....
.......O..
...O......
..........
.......O.O
...O.O....
.......O..
.........$
```
# Bonus:
For those seeking more challenge. Instead of using input swamps you will generate a swamp. Place the Ogre randomly. Place his gold randomly. Generate sinkholes based on the size of the swamp.

For example you are given N for a NxN swamp to generate. Generate a random swamp and apply your solution to it. The exact design/algorithm for random generation I leave it for you to tinker with. I suggest start with like 15% of the swamp spots are sinkholes and go up or down based on your results. (So you get paths and not always No Path)


----
## **DISCLAIMER**
This prompt has been adapted from [211 [Intermediate] Ogre Maze](https://www.reddit.com/r/dailyprogrammer/comments/33hwwf/20150422_challenge_211_intermediate_ogre_maze/
)

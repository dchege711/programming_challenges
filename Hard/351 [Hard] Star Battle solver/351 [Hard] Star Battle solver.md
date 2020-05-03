---
draft: true
----

# [351 (Hard) Star Battle solver](https://www.reddit.com/r/dailyprogrammer/comments/7xyi2w/20180216_challenge_351_hard_star_battle_solver/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Background
Star Battle is a grid-based logic puzzle. You are given a SxS square grid divided into S connected regions, and a number N. You must find the unique way to place N*S stars into the grid such that:

If you would like more information:

(https://www.gmpuzzles.com/blog/star-battle-rules-and-info/)
(https://www.youtube.com/watch?v=TB-RRMe82V4)
(http://starbattle-puzzle.com/tutorial/)
(https://www.gmpuzzles.com/blog/category/objectplacement/star-battle/)
# Challenge
Write a program to solve a Star Battle puzzle in a reasonable amount of time. There's no strict time requirement, but you should run your program through to completion for at least one (N, S) = (2, 10) puzzle for it to count as a solution.

Feel free to use whatever input/output format is most convenient for you. In the examples below, first N is given, then the SxS grid is given, with each cell labeled by a letter corresponding to its region. The output is . for empty cells and * for cells containing a star. But you don't have to use this format.


```
.
```

```
*
```
# Example input (N, S) = (1, 6)

```
1
AABBCC
AABCCC
AABCCC
DDBBEE
DDBBEF
DDBBFF
```
Source

(http://puzzleparasite.blogspot.com/2012/09/rules-star-battle.html)
# Example output

```
..*...
*.....
...*..
.....*
.*....
....*.
```
# Challenge input (N, S) = (2, 10)

```
2
AAAABBCCCC
ADAABBBCBB
ADDBBBBBBB
DDDDBEEEEB
DDBBBBBBEB
FFFFGGHHHH
FIFFGGGHGG
FIIGGGGGGG
IIIIGJJJJG
IIGGGGGGJG
```
by Bryce Herdt

(https://www.gmpuzzles.com/blog/2015/03/star-battle-bryce-herdt/)
# Bonus input (N, S) = (3, 15)

```
3
AAAAABBBBBCCCCC
ADDDDBBBBBEEECC
ADDDDDDBEEEEEEC
ADDDFDDBEEGEEEC
ADDFFFHHHGGGEEC
AAAFFFHHHGGGCCC
AAHHFHHIHHGHCCC
AAAHHHIIIHHHJJJ
AAAKKKIIIKKKKLJ
AAAMKKKIKKKMLLJ
NNNMMKKKKKMMLLJ
NNNOMMMMMMMLLLJ
NOOOOMMMMMOOLLL
NOOOOOMMMOOOLLL
NNOOOOOOOOOOLLL
```
by Thomas Snyder

(https://www.gmpuzzles.com/blog/2013/08/dr-sudoku-prescribes-125-star-battle/)

----
## **DISCLAIMER**
This prompt has been adapted from [351 [Hard] Star Battle solver](https://www.reddit.com/r/dailyprogrammer/comments/7xyi2w/20180216_challenge_351_hard_star_battle_solver/)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [142 (Easy) Falling Sand](https://www.reddit.com/r/dailyprogrammer/comments/1rdtky/111113_challenge_142_easy_falling_sand/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Easy): Falling Sand
(#EasyIcon)
Falling-sand Games are particle-simulation games that focus on the interaction between particles in a 2D-world. Sand, as an example, might fall to the ground forming a pile. Other particles might be much more complex, like fire, that might spread depending on adjacent particle types.

(http://en.wikipedia.org/wiki/Falling-sand_game)
Your goal is to implement a mini falling-sand simulation for just sand and stone. The simulation is in 2D-space on a uniform grid, where we are viewing this grid from the side. Each type's simulation properties are as follows:

# Formal Inputs & Outputs
## Input Description
On standard console input, you will be given an integer N which represents the N x N grid of ASCII characters. This means there will be N-lines of N-characters long. This is the starting grid of your simulated world: the character ' ' (space) means an empty space, while '.' (dot) means sand, and '#' (hash or pound) means stone. Once you parse this input, simulate the world until all particles are settled (e.g. the sand has fallen and either settled on the ground or on stone). "Ground" is defined as the solid surface right below the last row.

## Output Description
Print the end result of all particle positions using the input format for particles.

# Sample Inputs & Outputs
## Sample Input

```
5
.....
  #  
#    

    .
```
## Sample Output

```
.  
. #  
#    
    .
 . ..
```

----
## **DISCLAIMER**
This prompt has been adapted from [142 [Easy] Falling Sand](https://www.reddit.com/r/dailyprogrammer/comments/1rdtky/111113_challenge_142_easy_falling_sand/
)

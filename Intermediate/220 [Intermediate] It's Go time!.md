# [220 (Intermediate) It's Go time!](https://www.reddit.com/r/dailyprogrammer/comments/3axjac/20150624_challenge_220_intermediate_its_go_time/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): It's Go time!
(#IntermediateIcon)
Go is a board game involving placing black and white stones on a grid. Two opponents take turns to place stones; one player places white stones, the other black. Stones of the same colour form a group, as long as they're all connected via the cardinal axes. The leftmost pair of stones (represented by #) below are valid groups, and the rightmost pair are not.

(https://en.wikipedia.org/wiki/Go_(game))

```
#
```

```
#      ###   #     ##  
###    # #   #      ##  
 ##    ###    ##      ## 
  #     #      #       ##
```
Now, when a player places stones such that a group of the opponent's colour is touching no more open spaces (liberties), then that group is removed from play. The edges of the board do not count as open spaces. Let the black stones be represented by b and white stones by w. Here, the player plays as the black stones.


```
b
```

```
w
```

```
bbbbb
 wwwb
bwbwb
 bbbb
```
Let B be the stone I place in the next turn. If I place the stone here:


```
B
```

```
bbbbb
Bwwwb
bwbwb
 bbbb
```
The white group is entirely enclosed by the black group, and so the white group is removed from play.
If a situation were to arise where both your own and your opponent's stones would be removed, your opponent's stones would be removed first, and then (only if your stones still need to be removed) your own stones would be removed.

Liberties don't need to be outside of the group; they can be inside the group, too. These are called eyes. Here, the white group survives, as it has the eye:


```
bbbbb
bbwwwwb
bww wb
 bwwwwb
  bbbbb
```
Your challenge today is to determine the location on the board which, when a stone of your own colour is placed there, will remove the greatest number of your opponent's stones.

# Formal Inputs and Outputs
## Input Description
You will be given the size of the grid as a width and a height. Next, you will be given the player's colour - either b or w. Finally, you will be given a grid of the appropriate dimensions, using the format I used in the Description: spaces for empty grid regions, and b and w for stones of either colour.


```
b
```

```
w
```

```
b
```

```
w
```
## Output Description
Output the co-ordinate of the location which, if you were to place a stone of your own colour there, would result in the greatest number of your opponent's stones being removed. The top-left corner is location (0, 0).


```
(0, 0)
```
# Sample Inputs and Outputs
### Input

```
7 5
b      
 bbbbb 
bbwwwwb
bww wb 
 bwwwwb
  bbbbb
```
### Output

```
(3, 2)
```
### Input

```
9 11
w
    ww   
  wwbbbw 
  wbbbbw 
 wwbbbbw 
 wwwwwww 
 wbbbbww 
 wbwbbww 
 wbwbbww 
 wwwbbww 
    wbw  
    w
```
### Output

```
(5, 10)
```
### Input

```
7 7
w
w w w w
 bbbbb 
wbbbbbw
 bbbbb 
wbbbbbw
 bbbbb 
w w w w
```
### Output

```
No constructive move
```
## Sample 4
### Input

```
4 3
b
 bw 
bw w
 bw
```
### Output

```
(2, 1)
```
## Sample 5
(thanks to /u/adrian17)

(/u/adrian17)
### Input

```
7 5
b
 bb bb 
bww wwb
 bbbbb 
bwwwb  
 bb
```
### Output

```
(3, 1)
```
# Notes
I apologise beforehand to any Go players for presenting such unrealistic scenarios!

Got any cool challenge ideas? Post them to /r/DailyProgrammer_Ideas!

(/r/DailyProgrammer_Ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [220 [Intermediate] It's Go time!](https://www.reddit.com/r/dailyprogrammer/comments/3axjac/20150624_challenge_220_intermediate_its_go_time/
)

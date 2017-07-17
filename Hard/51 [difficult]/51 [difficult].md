# [51 (difficult)](https://www.reddit.com/r/dailyprogrammer/comments/ti5jn/5112012_challenge_51_difficult/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Take a 7x7 grid of cells and remove the central cell (like a chessboard, but slightly smaller and with a hole in the middle), and it would look something like this. The number of cells is 7*7 - 1 = 48 because we removed the central cell.

(http://i.imgur.com/UXtTA.png)
Now, lets start tiling this grid with dominoes. Each domino covers exactly two cells that are either horizontally or vertically next to each other, so if you are going to tile the whole 
thing with dominoes, you would need 24 of them (48 over 2). Here is an example of the grid being perfectly tiled by dominoes. There are exactly 75272 ways you can use dominoes to tile a 7x7 grid with the central cell removed. 

(http://i.imgur.com/NmD8m.png)
Find the last 8 digits of the number of ways you can use dominoes to tile a 15x15 grid with the central cell removed. 

Note: rotations and reflections of tilings count as distinct tilings. I.e. if two tilings differ only by rotation or reflection, they are still considered to be different. 


----
## **DISCLAIMER**
This prompt has been adapted from [51 [difficult]](https://www.reddit.com/r/dailyprogrammer/comments/ti5jn/5112012_challenge_51_difficult/
)

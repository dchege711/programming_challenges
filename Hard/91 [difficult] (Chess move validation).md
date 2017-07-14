# [91 (difficult) (Chess move validation)](https://www.reddit.com/r/dailyprogrammer/comments/yqyrs/8242012_challenge_91_difficult_chess_move/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Forsyth-Edwards notation is a is a notation used by chess players for describing a particular board position of a chess game. It contains information about the pieces, whose turn it is, who can castle, and how many turns have passed, among others. Write a program that reads a FEN file and two coordinates from input, like this:

(http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation)

```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
e2 e4
```
Your program parses the FEN board, then determines whether moving the piece on coordinate 1 to coordinate 2 is a valid move, printing either true or false. As demonstrated here, it is:


```
true
```

```
false
```

|/|a|b|c|d|e|f|g|h|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|8|♜|♞|♝|♛|♚|♝|♞|♜|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|7|♟|♟|♟|♟|♟|♟|♟|♟|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|6|·|·|·|·|·|·|·|·|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|5|·|·|·|·|·|·|·|·|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|4|·|·|·|·|♙|·|·|·|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|3|·|·|·|·|↑|·|·|·|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|2|♙|♙|♙|♙|◌|♙|♙|♙|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|1|♖|♘|♗|♕|♔|♗|♘|♖|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
|
----
## **DISCLAIMER**
This prompt has been adapted from [91 [difficult] (Chess move validation)](https://www.reddit.com/r/dailyprogrammer/comments/yqyrs/8242012_challenge_91_difficult_chess_move/
)

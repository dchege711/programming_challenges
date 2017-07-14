# [155 (Intermediate) We're about to score!](https://www.reddit.com/r/dailyprogrammer/comments/21ejqz/2632014_challenge_155_intermediate_were_about_to/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
One of the ways that chess games are tracked during play is to assign values to each piece and then look at the pieces that remain on the board for each player. After several moves where pieces have been taken, one can quickly determine who has an advantage.

Pieces are assigned standard valuations: 

More info on chess values can be seen HERE

(http://en.wikipedia.org/wiki/Chess_piece_relative_value)
# Input Description
Each line of input will be given in standard chess algebraic notation: 

Here's a picture of the notation to give you an idea : Image

(http://home.comcast.net/%7Edanheisman/images/Record_board.jpg)
Pieces (except for pawns) have a capital letter associated with them:

King = K; Knight = N; Queen = Q; Rook = R; Bishop = B; None = pawns, they are just specified by their file. 

Captures are marked with an "x": 

e.g. "Qxe5" for "queen captures the piece on square e5"; pawn captures are given by file, for example "exd5". 

Castling is indicated as such: O-O for kingside, O-O-O Queenside. Check is indicated by a "+" and checkmate is given by "mate" or "#". 

For more help on chess notation see HERE

(http://home.comcast.net/%7Edanheisman/Articles/recording_chess.htm)
# Formal Input Description
Three values per line: move number, then white's move, then black's move using chess algebraic notation.

Example:

etc...

# Formal Output Description
Your program should emit two values, one for white and one for black, at the end of the series of moves (for an incomplete game).

# Sample Input
This is actually Anand v Carlsen from the Zurich Chess Challenge 2014, round 5 play.

# Sample output
12-12

# Challenge Input
This is actually Aronian vs So from the 2014 76th Tata Steel Masters round 6. Aronian would go on to win.

# Thanks
Big thank you to /u/jnazario for the submission and for his stream of posts over at /r/dailyprogrammer_ideas

(/u/jnazario)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [155 [Intermediate] We're about to score!](https://www.reddit.com/r/dailyprogrammer/comments/21ejqz/2632014_challenge_155_intermediate_were_about_to/
)

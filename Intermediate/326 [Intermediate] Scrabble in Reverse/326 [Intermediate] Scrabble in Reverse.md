# [326 (Intermediate) Scrabble in Reverse](https://www.reddit.com/r/dailyprogrammer/comments/6sld01/20170809_challenge_326_intermediate_scrabble_in/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Many of us have played Scrabble, the game where you lay down tiles of letters on a board to form interlocking valid English language words. Players get points depending on the tiles they play and the bonus squares they use per word. 

Now, can you reverse a Scrabble game? That is, given a board can you infer what words were played and in what order?

Given some basic rules of Scrabble:

For your dictionary, use any standard English language dictionary (or enable1.txt).

(https://github.com/dolph/dictionary/blob/master/enable1.txt)
# Example Input
You'll be given two integers on a line telling you how many rows and columns to read, then a board (with those dimensions) with words filled out, with blank spaces using a period .. Example:


```
.
```

```
7 8
...cite
.tilt..
...e...
.planes
...n...
.......
.......
```
# Example Output
Your program should emit one or more words, in the order in which they were played (first to last). Example:


```
planes
clean
cite
tilt
```
An alternative could be:


```
planes
clean
tilt
cite
```
# Challenge Input

```
9 10
.........
.........
.ferries.
.l.....t.
.o..an.a.
.e...e.f.
.short.f.
.......e.
..called.
```
# Challenge Output

```
an
net
short
floes
ferries
staffed
called
```

----
## **DISCLAIMER**
This prompt has been adapted from [326 [Intermediate] Scrabble in Reverse](https://www.reddit.com/r/dailyprogrammer/comments/6sld01/20170809_challenge_326_intermediate_scrabble_in/
)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [162 (Intermediate) Novel Compression, pt. 2](https://www.reddit.com/r/dailyprogrammer/comments/25hlo9/5142014_challenge_162_intermediate_novel/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Intermediate): Novel Compression, pt. 2: Compressing the Data
(#IntermediateIcon)
Welcome to Part 2 of this week's Theme Week. Today we are (predictably) doing the opposite of Monday's challenge. We will be taking uncompressed data, running it through a compression algorithm, and printing compressed data. The grammar and format is exactly the same as last time.

You are still advised to write your program in a way that can be easily adapted and extended later on. A challenge later this week will involve putting all of your work together into a fully featured program!

# Formal Inputs and Outputs
## Input Description
The input will simply be uncompressed textual data. At the end, an EOF symbol is printed (note: in Windows an EOF is entered using Ctrl-Z on the console, and in Linux an EOF is entered using Ctrl-D at a terminal - or alternatively pipe a file containing the input using cat.)


```
cat
```
## Data Format
Same rules as before. All words must go into a dictionary (just a list of words.)

(/r/dailyprogrammer/comments/25clki/5122014_challenge_162_easy_novel_compression_pt_1/)
If a lower-case word (eg. stanley) is encountered, print its index in the dictionary, followed by a space.


```
stanley
```
If a capitalised word (first letter is upper-case, eg. Stanley) is encountered, print its index in the dictionary, followed by a caret (^), followed by a space.


```
Stanley
```

```
^
```
If an upper-case word (eg. Stanley) is encountered, print its index in the dictionary, followed by an exclamation point (!), followed by a space.


```
Stanley
```

```
!
```
If the previous and next words encountered are joined by a hyphen rather than a space (eg. hunter-gatherer), print a hyphen (-), followed by a space (eg. 44 - 47).


```
hunter-gatherer
```

```
-
```

```
44 - 47
```
If word is followed by any of the following symbols: . , ? ! ; :, print that symbol after it, followed by another space (eg. 44 !).


```
. , ? ! ; :
```

```
44 !
```
If a new line is encountered, print the letter R, followed by a space.


```
R
```
If the end of the input has been reached, print the letter E, followed by a space.


```
E
```
Note: All words will be in the Latin alphabet.

Now for an important bit. If you encounter any of the following:

A word is capitalised in any other different way than above,

A word is not alphabetical (eg. has numbers in it),

A symbol not in . , ? ! ; : is encountered,


```
. , ? ! ; :
```
Two or more symbols are next to each other like ??1),


```
??1
```
Then you must print an error message and then stop, because our simple basic compression format cannot account for these cases. Normally a practical compression system would handle it more gracefully, but this is just a challenge after all so just drop them.

## Example Data
Therefore, if our input is given as:


```
The quick brown fox jumps over the lazy dog.
Or, did it?
```
Then the output data is:


```
11
the
quick
brown
fox
jumps
over
lazy
dog
or
did
it
0^ 1 2 3 4 5 0 6 7 . R 8^ , 9 10 ? E
```
## Output Description
Print the resultant data from your compression algorithm, using the rules described above.

# Challenge
## Challenge Input

```
I would not, could not, in the rain.
Not in the dark. Not on a train.
Not in a car. Not in a tree.
I do not like them, Sam, you see.
Not in a house. Not in a box.
Not with a mouse. Not with a fox.
I will not eat them here or there.
I do not like them anywhere!
```
## Example Challenge Output
Your output may vary slightly depending on how you populate your word dictionary.


```
30
i
would
not
could
in
the
rain
dark
on
a
train
car
tree
do
like
them
sam
you
see
house
box
with
mouse
fox
will
eat
here
or
there
anywhere
0^ 1 2 , 3 2 , 4 5 6 . R 2^ 4 5 7 . 2^ 8 9 10 . R 2^ 4 9 11 . 2^ 4
9 12 . R 0^ 13 2 14 15 , 16^ , 17 18 . R 2^ 4 9 19 . 2^ 4 9 20 . R 2^ 21 9
22 . 2^ 21 9 23 . R 0^ 24 2 25 15 26 27 28 . R 0^ 13 2 14 15 29 ! R E
```

----
## **DISCLAIMER**
This prompt has been adapted from [162 [Intermediate] Novel Compression, pt. 2](https://www.reddit.com/r/dailyprogrammer/comments/25hlo9/5142014_challenge_162_intermediate_novel/
)

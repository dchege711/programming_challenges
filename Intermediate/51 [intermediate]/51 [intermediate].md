---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [51 (intermediate)](https://www.reddit.com/r/dailyprogrammer/comments/ti5ji/5112012_challenge_51_intermediate/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Brainfuck is an extremely minimalistic programming language. The memory consists of a large array of bytes, the "tape", 
which is manipulated by moving around a single tape pointer. The 8 commands are:


|Character|Action|
| --- | --- |
|<|move the pointer to the left|
| --- | --- |
|>|move the pointer to the right|
| --- | --- |
|+|increment the byte the pointer is pointing at (mod 256)|
| --- | --- |
|-|decrement the byte the pointer is pointing at (mod 256)|
| --- | --- |
|[|if the data which the tape pointer is pointing at is 0, jump forward to the command after the matching square bracket. Otherwise, just continue to the next command|
| --- | --- |
|]|if the data which the tape pointer is pointing at is not 0, jump backwards to the command after the matching square bracket. Otherwise, just continue to the next command|
| --- | --- |
|,|read a character from the input and store it into the current pointer byte|
| --- | --- |
|.|output the current pointer byte as an ascii character|
| --- | --- |
|Any other character is ignored and treated as a comment

[ ... ] thus make a kind of while loop, equivalent to something like "while(data[pointer] != 0) { ... }". 
The brackets match like parentheses usually do, each starting one has a matching ending one. These loops can be nested inside other loops. 


```
[ ... ]
```
Write a program that reads a brainfuck program and its input, interprets the code, and returns the output.

More information, including a "Hello World" program, can be found on wikipedia. 

(http://en.wikipedia.org/wiki/Brainfuck)
If you've written your program successfully, try running this and see what pops out:


```
++++++++++[>>++++++>+++++++++++>++++++++++>+++++++++>+++>+++++>++++>++++++++>+[<
]<-]>>+++++++.>+.-.>+++.<++++.>>+++++++.<<++.+.>+++++.>.<<-.>---.<-----.-.+++++.
>>>+++.-.<<-.<+..----.>>>>++++++++.>+++++++..<<<<+.>>>>-.<<<<.++++.------.<+++++
.---.>>>>>.<<<++.<<---.>++++++.>>>>+.<<<-.--------.<<+.>>>>>>+++.---.<-.<<<<---.
<.>---.>>>>>>.
```
(http://www.reddit.com/user/nooodl)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [51 [intermediate]](https://www.reddit.com/r/dailyprogrammer/comments/ti5ji/5112012_challenge_51_intermediate/
)

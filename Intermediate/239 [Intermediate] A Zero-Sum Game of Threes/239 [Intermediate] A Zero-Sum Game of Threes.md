---
draft: true
----

# [239 (Intermediate) A Zero-Sum Game of Threes](https://www.reddit.com/r/dailyprogrammer/comments/3rhzdj/20151104_challenge_239_intermediate_a_zerosum/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Let's pursue Monday's Game of Threes further!

(https://www.reddit.com/r/dailyprogrammer/comments/3r7wxz/20151102_challenge_239_easy_a_game_of_threes/)
To make it more fun (and make it a 1-player instead of a 0-player game), let's change the rules a bit: You can now add any of [-2, -1, 1, 2] to reach a multiple of 3. This gives you two options at each step, instead of the original single option. 

(https://en.wikipedia.org/wiki/Zero-player_game)
With this modified rule, find a Threes sequence to get to 1, with this extra condition: The sum of all the numbers that were added must equal 0. If there is no possible correct solution, print Impossible.


```
Impossible
```
# Sample Input:

```
929
```
# Sample Output:

```
929 1
310 -1
103 -1
34 2
12 0
4 -1
1
```
Since 1 - 1 - 1 + 2 - 1 == 0, this is a correct solution. 


```
1 - 1 - 1 + 2 - 1 == 0
```
# Bonus points
Make your solution work (and run reasonably fast) for numbers up to your operating system's maximum long int value, or its equivalent. For some concrete test cases, try:


```
18446744073709551615
```

```
18446744073709551614
```

----
## **DISCLAIMER**
This prompt has been adapted from [239 [Intermediate] A Zero-Sum Game of Threes](https://www.reddit.com/r/dailyprogrammer/comments/3rhzdj/20151104_challenge_239_intermediate_a_zerosum/
)

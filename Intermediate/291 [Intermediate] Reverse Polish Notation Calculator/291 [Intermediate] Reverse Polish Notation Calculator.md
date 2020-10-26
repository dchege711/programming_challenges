---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [291 (Intermediate) Reverse Polish Notation Calculator](https://www.reddit.com/r/dailyprogrammer/comments/5c5jx9/20161109_challenge_291_intermediate_reverse/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

A little while back we had a programming challenge to convert an infix expression (also known as "normal" math) to a postfix expression (also known as Reverse Polish Notation). Today we'll do something a little different: We will write a calculator that takes RPN input, and outputs the result.

(https://www.reddit.com/r/dailyprogrammer/comments/2yquvm/20150311_challenge_205_intermediate_rpn/)
(https://en.wikipedia.org/wiki/Reverse_Polish_notation)
# Formal input
The input will be a whitespace-delimited RPN expression. The supported operators will be:


```
+
```

```
-
```

```
*
```

```
x
```

```
/
```

```
3/2=1.5
```

```
3/2=1
```

```
//
```

```
3/2=1
```

```
%
```

```
14%3=2
```

```
21%7=0
```

```
^
```

```
!
```
Sample input:


```
0.5 1 2 ! * 2 1 ^ + 10 + *
```
# Formal output
The output is a single number: the result of the calculation. The output should also indicate if the input is not a valid RPN expression.

Sample output:


```
7
```
Explanation: the sample input translates to 0.5 * ((1 * 2!) + (2 ^ 1) + 10), which comes out to 7.


```
0.5 * ((1 * 2!) + (2 ^ 1) + 10)
```

```
7
```
## Challenge 1
Input: 1 2 3 4 ! + - /  100 *


```
1 2 3 4 ! + - /  100 *
```
Output: -4


```
-4
```
## Challenge 2
Input: 100 807 3 331 * + 2 2 1 + 2 + * 5 ^ * 23 10 558 * 10 * + + *


```
100 807 3 331 * + 2 2 1 + 2 + * 5 ^ * 23 10 558 * 10 * + + *
```
# Finally...
Hope you enjoyed today's challenge! Have a fun problem or challenge of your own? Drop by /r/dailyprogrammer_ideas and share it with everyone!

(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [291 [Intermediate] Reverse Polish Notation Calculator](https://www.reddit.com/r/dailyprogrammer/comments/5c5jx9/20161109_challenge_291_intermediate_reverse/
)

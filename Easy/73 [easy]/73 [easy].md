---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [73 (easy)](https://www.reddit.com/r/dailyprogrammer/comments/w4l6e/762012_challenge_73_easy/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

During the 70s and 80s, some handheld calculators used a very different notation for arithmetic called Reverse Polish notation (RPN). Instead of putting operators (+, *, -, etc.) between their operands (as in 3 + 4), they were placed behind them: to calculate 3 + 4, you first inputted the operands (3 4) and then added them together by pressing +.

(http://en.wikipedia.org/wiki/Reverse_Polish_notation)

```
+
```

```
*
```

```
-
```

```
3 + 4
```

```
3 + 4
```

```
3 4
```

```
+
```
Internally, this was implemented using a stack: whenever you enter a number, it's pushed onto the stack, and whenever you enter an operator, the top two elements are popped off for the calculation. Here's an example of a RPN calculator calculating 3 4 * 6 2 - +:


```
3 4 * 6 2 - +
```

```
[3] --> 3
[4] --> 3 4
[*] --> 12      ( 3 * 4 = 12)
[6] --> 12 6
[2] --> 12 6 2
[-] --> 12 4    ( 6 - 2 =  4)
[+] --> 16      (12 + 4 = 16)
```
Your task is to implement a program that reads a string in Reverse Polish notation and prints the result of the calculation. Your program should support positive and negative integers and the operators +, -, *. (For extra credit, you can implement extra functions, such as decimal numbers, division, exponentiation, etc.)


```
+
```

```
-
```

```
*
```

----
## **DISCLAIMER**
This prompt has been adapted from [73 [easy]](https://www.reddit.com/r/dailyprogrammer/comments/w4l6e/762012_challenge_73_easy/
)

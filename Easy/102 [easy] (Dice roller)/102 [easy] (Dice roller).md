---
draft: true
----

# [102 (easy) (Dice roller)](https://www.reddit.com/r/dailyprogrammer/comments/10pf0j/9302012_challenge_102_easy_dice_roller/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

In tabletop role-playing games like Dungeons & Dragons, people use a system called dice notation to represent a combination of dice to be rolled to generate a random number. Dice rolls are of the form AdB (+/-) C, and are calculated like this:

(http://en.wikipedia.org/wiki/Dice_notation)
If A is omitted, its value is 1; if (+/-)C is omitted, step 2 is skipped. That is, "d8" is equivalent to "1d8+0".


```
"d8"
```

```
"1d8+0"
```
Write a function that takes a string like "10d6-2" or "d20+7" and generates a random number using this syntax.


```
"10d6-2"
```

```
"d20+7"
```
Here's a hint on how to parse the strings, if you get stuck:


```
Split the string over 'd' first; if the left part is empty, A = 1,
otherwise, read it as an integer and assign it to A. Then determine
whether or not the second part contains a '+' or '-', etc.
```

----
## **DISCLAIMER**
This prompt has been adapted from [102 [easy] (Dice roller)](https://www.reddit.com/r/dailyprogrammer/comments/10pf0j/9302012_challenge_102_easy_dice_roller/
)

# [98 (difficult) (Reading digital displays)](https://www.reddit.com/r/dailyprogrammer/comments/zx9l4/9152012_challenge_98_difficult_reading_digital/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Challenge #92 [easy] involved converting a number to a seven segment display representation (of a variable size) using +, -, and |. Assume the font looks like this:

(http://www.reddit.com/r/dailyprogrammer/comments/ywlvf/8272012_challenge_92_easy_digital_number_display/)

```
+ +--+ +--+ +  + +--+ +--+ +--+ +--+ +--+ +--+ 
   |    |    | |  | |    |       | |  | |  | |  | 
   |    |    | |  | |    |       | |  | |  | |  | 
   + +--+ +--+ +--+ +--+ +--+    + +--+ +--+ +  + 
   | |       |    |    | |  |    | |  |    | |  | 
   | |       |    |    | |  |    | |  |    | |  | 
   + +--+ +--+    + +--+ +--+    + +--+ +--+ +--+
```
Write a program that reads such a string and converts it back into a number. (You'll have to deduce the size yourself.) The output for the above text would be 1234567890.


```
1234567890
```
As a bonus, have your program be able to read a file containing characters of different sizes, like this:


```
+-+ +  + +-+
  | |  | |
+-+ |  | +-+
  | +--+   |
+-+    | +-+
       |
       +
```

----
## **DISCLAIMER**
This prompt has been adapted from [98 [difficult] (Reading digital displays)](https://www.reddit.com/r/dailyprogrammer/comments/zx9l4/9152012_challenge_98_difficult_reading_digital/
)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [58 (easy)](https://www.reddit.com/r/dailyprogrammer/comments/u8jmi/5282012_challenge_58_easy/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

As computer programmers are well aware, it can be very useful to write numbers using numerical bases other than the familiar base 10 notation we use in everyday life. In computer programming, base 2 and base 16 are especially handy. In base 2, the number 1234 becomes 10011010010 and in base 16 it becomes 4D2. 

(http://en.wikipedia.org/wiki/Binary_number)
(http://en.wikipedia.org/wiki/Hexadecimal)
Because there are only 10 regular digits, when numbers are written in base 16, the first few letters of the alphabet are added to represent the remaining required digits, so 'A' stands in for 10, 'B' for 11, 'C' for 12, 'D' for 13, 'E' for 14 and 'F' for 15. 

Of course, this trick of adding letters to stand in for numbers allows us to represent higher bases than 16; if you can use all letters of the alphabet, you can represent bases up to base 36 (because there are ten regular digits and 26 "letter-digits"). So for instance, 12345678 becomes 1L2FHE in base 23 and 4IDHAA in base 19. 

Write a program that will take a number and convert it to any base between 2 and 36. Have the program print out 19959694 in base 35 and 376609378180550 in base 29. 

NOTE: Many languages have this built in as a library function. For instance, in Java, the function Integer.toString(i, radix) does exactly this. However, the entire point of this challenge is to write the program yourself, so you are not allowed to use any library functions like this.

BONUS: A number is said to be "palindromic in base N" if, when written in base N the number is the same backwards and forwards. So, for instance, the number 16708 is palindromic in base 27, because in base 27 the number is written as MOM, obviously a palindrome. The number 12321 is a palindrome in in base 10, because 12321 written backwards is 12321. Some numbers are palindromic in several bases, the number 15167 for instance is palindromic in bases 9, 27, 28, 35 and 36. 

In what bases is the number 10858 palindromic?

(http://www.reddit.com/user/Hannoii)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [58 [easy]](https://www.reddit.com/r/dailyprogrammer/comments/u8jmi/5282012_challenge_58_easy/
)

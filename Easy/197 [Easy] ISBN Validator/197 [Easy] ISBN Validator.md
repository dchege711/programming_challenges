---
draft: true
----

# [197 (Easy) ISBN Validator](https://www.reddit.com/r/dailyprogrammer/comments/2s7ezp/20150112_challenge_197_easy_isbn_validator/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
ISBN's (International Standard Book Numbers) are identifiers for books. Given the correct sequence of digits, one book can be identified out of millions of others thanks to this ISBN. But when is an ISBN not just a random slurry of digits? That's for you to find out.

# Rules
Given the following constraints of the ISBN number, you should write a function that can return True if a number is a valid ISBN and False otherwise.

An ISBN is a ten digit code which identifies a book. The first nine digits represent the book and the last digit is used to make sure the ISBN is correct.

To verify an ISBN you :-

For example :

0-7475-3269-9 is Valid because 

(10 * 0) + (9 * 7) + (8 * 4) + (7 * 7) + (6 * 5) + (5 * 3) + (4 * 2) + (3 * 6) + (2 * 9) + (1 * 9) = 242 which can be divided by 11 and have no remainder. 

For the cases where the last digit has to equal to ten, the last digit is written as X. For example 156881111X.

# Bonus
Write an ISBN generator. That is, a programme that will output a valid ISBN number (bonus if you output an ISBN that is already in use :P )

# Finally
Thanks to /u/TopLOL for the submission!

(/u/TopLOL)

----
## **DISCLAIMER**
This prompt has been adapted from [197 [Easy] ISBN Validator](https://www.reddit.com/r/dailyprogrammer/comments/2s7ezp/20150112_challenge_197_easy_isbn_validator/
)

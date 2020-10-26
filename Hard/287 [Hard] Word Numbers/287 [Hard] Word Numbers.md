---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [287 (Hard) Word Numbers](https://www.reddit.com/r/dailyprogrammer/comments/57fzcv/20161014_challenge_287_hard_word_numbers/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Read the problem carefully and make sure you understand it. This is a hard problem, so if it seems straightforward, you might be misreading something. Feel free to ask for clarification.

Consider the following procedure:

(https://en.wikipedia.org/wiki/Alphabetical_order)
Now, you probably can't actually do this procedure. It would take too long or require too much memory. But determine what, if you did this procedure, would be the answers to the following questions about your final, 51-billion-letter string:

You must actually be able to answer all these questions. Writing a program that would theoretically find the answer given a long time is not a valid solution to this problem. There's no strict runtime limit, but actually run your program to completion and get the answers before posting your code. (If you want a goal, my Python solution takes 0.05 seconds, but that fast is not necessary.)

# Details
When you write the numbers out in step 2, omit spaces, punctuation, and the word "and". Examples of how this step looks:


```
100 -> onehundred
1709 -> onethousandsevenhundrednine
500000000 -> fivehundredmillion
911610034 -> ninehundredelevenmillionsixhundredtenthousandthirtyfour
```
The first word in this list after sorting alphabetically is eight, followed by eighteen, then eighteenmillion, then eighteenmillioneight. Thus your final string will begin like this:


```
eight
```

```
eighteen
```

```
eighteenmillion
```

```
eighteenmillioneight
```

```
eighteighteeneighteenmillioneighteenmillioneight...
```
And be 51 billion letters long.

# Example
The procedure requires taking the first 51 billion letters in step 5. As an example, if instead I asked you to take the first 28 letters in step 5, then your final string would be:


```
eighteighteeneighteenmillion
```
And the answers to the three questions would be:

# Bonus
Same procedure, except start with the integers 1 through 999,999,999,999 in step 1, and take the first 68 trillion (68,000,000,000,000) letters in step 5. If I did it right (that's a big "if"), this will also end on a number name boundary.

# Notes
This is an old ITA Software hiring puzzle, and the solution can be found in several places on the web (including Reddit). So if you go looking for it, spoiler alert! On the other hand, it's easy to check your solution by doing a web search for your answer to question #3.

Thanks to u/wizao for posting this challenge to r/dailyprogrammer_ideas!

(/u/wizao)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [287 [Hard] Word Numbers](https://www.reddit.com/r/dailyprogrammer/comments/57fzcv/20161014_challenge_287_hard_word_numbers/
)

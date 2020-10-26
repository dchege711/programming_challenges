---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [189 (Easy) Hangman!](https://www.reddit.com/r/dailyprogrammer/comments/2mlfxp/20141117_challenge_189_easy_hangman/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

We all know the classic game hangman, today we'll be making it. With the wonderful bonus that we are programmers and we can make it as hard or as easy as we want. here is a wordlist to use if you don't already have one. That wordlist comprises of words spanning 3 - 15+ letter words in length so there is plenty of scope to make this interesting!

(http://www.joereynoldsaudio.com/wordlist.txt)
# Rules
For those that don't know the rules of hangman, it's quite simple.

There is 1 player and another person (in this case a computer) that randomly chooses a word and marks correct/incorrect guesses.

The steps of a game go as follows:

(http://en.wikipedia.org/wiki/Hangman_%28game%29)
This carries on until either

or

# Formal inputs and outputs
## input description
Apart from providing a wordlist, we should be able to choose a difficulty to filter our words down further. For example, hard could provide 3-5 letter words, medium 5-7, and easy could be anything above and beyond!

On input, you should enter a difficulty you wish to play in.

## output description
The output will occur in steps as it is a turn based game. The final condition is either win, or lose.

# Clarifications

----
## **DISCLAIMER**
This prompt has been adapted from [189 [Easy] Hangman!](https://www.reddit.com/r/dailyprogrammer/comments/2mlfxp/20141117_challenge_189_easy_hangman/
)

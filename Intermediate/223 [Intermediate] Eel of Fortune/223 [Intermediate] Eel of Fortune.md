# [223 (Intermediate) Eel of Fortune](https://www.reddit.com/r/dailyprogrammer/comments/3ddpms/20150715_challenge_223_intermediate_eel_of_fortune/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
You work on the popular game show Eel of Fortune, where contestants take turns fishing live eels out of an aquarium for the opportunity to solve a word puzzle. The word puzzle works like the game Hangman. A secret word is obscured on the board. A player guesses a letter of the alphabet, and if that letter appears anywhere in the secret word, all of the times it appears in the secret word are revealed.

An unfortunate incident occurred on yesterday's show. The secret word was SYNCHRONIZED, and at one point the board was showing:


```
SYNCHRONIZED
```

```
S _ N _ _ _ O N _ _ _ D
```
As you can see, the letters on the board spelled "snond", which is of course an extremely offensive word for telemarketer in the Doldunian language. This incident caused ratings to immediately plummet in East Doldunia. The Eel of Fortune producers want the ability to identify "problem words" for any given offensive word.

Write a function that, given a secret word and an offensive word, returns true if the board could theoretically display the offensive word (with no additional letters) during the course of solving the secret word.

# Examples

```
problem("synchronized", "snond") -> true
problem("misfunctioned", "snond") -> true
problem("mispronounced", "snond") -> false
problem("shotgunned", "snond") -> false
problem("snond", "snond") -> true
```
# Optional challenges
(https://code.google.com/p/dotnetperls-controls/downloads/detail?name=enable1.txt)
Thanks to /u/AtlasMeh-ed for submitting this challenge on /r/dailyprogrammer_ideas!

(/u/AtlasMeh-ed)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [223 [Intermediate] Eel of Fortune](https://www.reddit.com/r/dailyprogrammer/comments/3ddpms/20150715_challenge_223_intermediate_eel_of_fortune/
)

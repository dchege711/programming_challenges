---
draft: true
----

# [223 (Easy) Garland words](https://www.reddit.com/r/dailyprogrammer/comments/3d4fwj/20150713_challenge_223_easy_garland_words/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
A garland word is one that starts and ends with the same N letters in the same order, for some N greater than 0, but less than the length of the word. I'll call the maximum N for which this works the garland word's degree. For instance, "onion" is a garland word of degree 2, because its first 2 letters "on" are the same as its last 2 letters. The name "garland word" comes from the fact that you can make chains of the word in this manner:

(http://blog.vivekhaldar.com/post/89763722591/garland-words)

```
onionionionionionionionionionion...
```
Today's challenge is to write a function garland that, given a lowercase word, returns the degree of the word if it's a garland word, and 0 otherwise.


```
garland
```
# Examples

```
garland("programmer") -> 0
garland("ceramic") -> 1
garland("onion") -> 2
garland("alfalfa") -> 4
```
# Optional challenges
(https://code.google.com/p/dotnetperls-controls/downloads/detail?name=enable1.txt)
Thanks to /u/skeeto for submitting this challenge on /r/dailyprogrammer_ideas!

(/u/skeeto)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [223 [Easy] Garland words](https://www.reddit.com/r/dailyprogrammer/comments/3d4fwj/20150713_challenge_223_easy_garland_words/
)

---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [100 (intermediate) ("Bad" Word Filter)](https://www.reddit.com/r/dailyprogrammer/comments/106gse/9202012_challenge_100_intermediate_bad_word_filter/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Write a program called 'censor' that takes in one argument on the command line.  This argument is the filename of a newline-separated wordlist of profanity such as

http://urbanoalvarez.es/blog/2008/04/04/bad-words-list/ or

(http://urbanoalvarez.es/blog/2008/04/04/bad-words-list/)
http://www.bannedwordlist.com/SwearWordResources.htm

(http://www.bannedwordlist.com/SwearWordResources.htm)
The program should then read a text from standard in, and print it out again, but replacing every instance of a word in the wordlist with a 'censored' version.
The censored version of a word has the same first character as the word, and the rest of the characters are replaced with '*'.  

For example, the 'censored' version of 'peter' would be 'p****'

Example: 


```
>echo 'You jerkface!' | censor badwords.txt
You j***face!
```

----
## **DISCLAIMER**
This prompt has been adapted from [100 [intermediate] ("Bad" Word Filter)](https://www.reddit.com/r/dailyprogrammer/comments/106gse/9202012_challenge_100_intermediate_bad_word_filter/
)

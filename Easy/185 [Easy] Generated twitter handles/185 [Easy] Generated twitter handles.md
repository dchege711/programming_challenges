---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [185 (Easy) Generated twitter handles](https://www.reddit.com/r/dailyprogrammer/comments/2jt4cx/10202014_challenge_185_easy_generated_twitter/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
For those that don't tweet or know the workings of Twitter, you can reply to 'tweets' by replying to that user with an @ symbol and their username. 

Here's an example from John Carmack's twitter.

(https://twitter.com/ID_AA_Carmack)
His initial tweet

@ID_AA_Carmack : "Even more than most things, the challenges in computer vision seem to be the gulf between theory and practice."

And a reply

@professorlamp : @ID_AA_Carmack Couldn't say I have too much experience with that

You can see, the '@' symbol is more or less an integral part of the tweet and the reply. Wouldn't it be neat if we could think of names that incorporate the @ symbol and also form a word?

e.g.

@tack -> (attack)

@trocious ->(atrocious)

# Formal Inputs & Outputs
## Input description
As input, you should give a word list for your program to scout through to find viable matches. The most popular word list is good ol' enable1.txt

(https://code.google.com/p/dotnetperls-controls/downloads/detail?name=enable1.txt)
/u/G33kDude  has supplied an even bigger text file. I've hosted it on my site over here , I recommend 'saving as' to download the file.

(/u/G33kDude)
(http://www.joereynoldsaudio.com/WordList.txt)
## Output description
Both outputs should contain the 'truncated' version of the word and the original word. For example.


```
@tack : attack
```
There are two outputs that we are interested in:

# Bonus
I think it would be even better if we could find words that have 'at' in them at any point of the word and replace it with the @ symbol. Most of these wouldn't be valid in Twitter but that's not the point here.

For example

r@@a -> (ratata)

r@ic@e ->(raticate)

dr@ ->(drat)

# Finally
Have a good challenge idea?

Consider submitting it to /r/dailyprogrammer_ideas

(/r/dailyprogrammer_ideas)
Thanks to /u/jnazario for the challenge!

(/u/jnazario)
Remember to check out our IRC channel. Check the sidebar for a link -->


----
## **DISCLAIMER**
This prompt has been adapted from [185 [Easy] Generated twitter handles](https://www.reddit.com/r/dailyprogrammer/comments/2jt4cx/10202014_challenge_185_easy_generated_twitter/
)

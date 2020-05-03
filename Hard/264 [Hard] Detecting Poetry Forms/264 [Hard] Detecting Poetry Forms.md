---
draft: true
----

# [264 (Hard) Detecting Poetry Forms](https://www.reddit.com/r/dailyprogrammer/comments/4gzeze/20160429_challenge_264_hard_detecting_poetry_forms/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
This is a variant of last week's intermediate challenge and was inspired by a comment from /u/Agent_Epsilon. In short, given a piece of poetry can you write a program to tell you what rhyme scheme it has?

(/u/Agent_Epsilon)
(https://en.wikipedia.org/wiki/Rhyme_scheme)
From that challenge: we'll use the SPHINX project from Carnegie Mellon University to detect if words rhyme. Use this pronouncing dictionary in conjunction with this phoneme description to find rhyming words. Note that the dictionary uses the ARPAbet phonetic transcription code and includes stress indicators for the vowel sounds.

(http://cmusphinx.sourceforge.net/)
(http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b)
(http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b.phones)
(https://en.wikipedia.org/wiki/Arpabet)
# Input Description
You'll be given a poem in plain text, with line breaks as expected. Example:


```
A bather whose clothing was strewed
  By winds that left her quite nude
  Saw a man come along
  And unless we are wrong
  You expected this line to be lewd.
```
# Output Description
Your program should emit the rhyme scheme found in the poem. From the above example:


```
aabba
```
(It's a Limerick.)

# Challenge Input

```
There once was a young lady named bright
  Whose speed was much faster than light
  She set out one day
  In a relative way
  And returned on the previous night.
```
## 

```
Once upon a midnight dreary, while I pondered, weak and weary,
  Over many a quaint and curious volume of forgotten lore—
  While I nodded, nearly napping, suddenly there came a tapping,
  As of some one gently rapping, rapping at my chamber door.
  "'Tis some visiter," I muttered, "tapping at my chamber door—
              Only this and nothing more."
```
## 

```
Brothers, who when the sirens roar
From office, shop and factory pour
'Neath evening sky;
By cops directed to the fug
Of talkie-houses for a drug,
Or down canals to find a hug
```
## 

```
Two roads diverged in a yellow wood,
And sorry I could not travel both
And be one traveler, long I stood
And looked down one as far as I could
To where it bent in the undergrowth;
```
# Challenge Output

```
aabba
abcbbb
aabccc
abaab
```

----
## **DISCLAIMER**
This prompt has been adapted from [264 [Hard] Detecting Poetry Forms](https://www.reddit.com/r/dailyprogrammer/comments/4gzeze/20160429_challenge_264_hard_detecting_poetry_forms/
)

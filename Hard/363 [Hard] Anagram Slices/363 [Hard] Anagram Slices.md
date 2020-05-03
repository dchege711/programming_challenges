---
draft: true
----

# [363 (Hard) Anagram Slices](https://old.reddit.com/r/dailyprogrammer/comments/8rcjx0/20180615_challenge_363_hard_anagram_slices/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

(Warning: I have not tried this myself and I have no idea if it's any fun.)

Today's challenge is an optimization problem. When this post is 7 days old, the user that has posted the best (shortest) solution will receive +1 gold medal flair. Ties will be broken by taking the lexicographically earliest solution.

Given an input set of strings, produce an output string. Every string in the input must be an anagram of some slice of the output. A slice in this context is a series of characters from the string separated by a fixed amount (i.e. anything that can be formed using Python's s[a:b:c] syntax). It's different from a substring in that you're allowed to skip characters, as long as you skip the same number of characters on each step.


```
s[a:b:c]
```
# Example input

```
one
two
three
four
five
six
seven
```
# Example output

```
oufrirvaewstnoeaxh (length: 18)
```
So for example, seven is an anagram of vesne, which is a slice of this output starting at offset 6 and taking every second letter. That is. s[6:16:2] = "vesne". Note that ten is not an anagram of any slice of this string, even though the letters all appear within it.


```
seven
```

```
vesne
```

```
s[6:16:2] = "vesne"
```

```
ten
```
# Challenge input
This list of 1000 randomly-chosen four-letter words from enable1.

(https://gist.githubusercontent.com/cosmologicon/0a4448e8fdb79ee620a68ed131eac58e/raw/a8831d08019f73e7d5a52042e2c4afe6fea70011/363-hard-words.txt)

----
## **DISCLAIMER**
This prompt has been adapted from [363 [Hard] Anagram Slices](https://old.reddit.com/r/dailyprogrammer/comments/8rcjx0/20180615_challenge_363_hard_anagram_slices/
)

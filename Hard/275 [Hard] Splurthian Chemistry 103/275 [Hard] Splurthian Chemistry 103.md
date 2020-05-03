---
draft: true
----

# [275 (Hard) Splurthian Chemistry 103](https://www.reddit.com/r/dailyprogrammer/comments/4t11c3/20160715_challenge_275_hard_splurthian_chemistry/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Background
The Splurth Council for Atoms and Atom-Related Paraphernalia has erupted into bickering, with everyone having an opinion on how to build the periodic table. Abcder the Wise demands alphabetical ordering, Zyxwur the Comely wants reverse-alphabetical, and Gsvpnnhq the Random really wants to pick the names. Can you make everyone happy?

See Wednesday's Intermediate challenge for the preference procedure of element symbols in Splurthian Chemistry. You can ignore capital letters for the purpose of this challenge.

(https://www.reddit.com/r/dailyprogrammer/comments/4so25w/20160713_challenge_275_intermediate_splurthian/)
# Requirements
Today's Hard challenge is an optimization problem. Here is a list of 10,000 random 8-character strings. These are candidate element names. You must select some subset of (up to 676) distinct items from this list. The requirements are:

(http://pastebin.com/raw/uQKZWbR4)
Post a link to your list on pastebin or github or Google docs or somewhere. Also post the code you used to generate your list, along with your score.

# Scoring
Your score is as follows: assign each element a symbol using the process in Wednesday's challenge. Reverse the list of symbols you get. Your score is the number of symbols at the beginning of the reversed list that are in alphabetical order.

# Example scoring
Here is a valid submission list that I generated. The first and last few entries are:

(http://pastebin.com/raw/XX7d3dx3)

```
aabmevmt
abhhwzpo
aehwwogz
afbvhlke
afycbvxv
agfigxja
agxdnjyc
....
xoyittxg
xrlkgqbe
xxutzias
ycykczyb
ygnoizht
yivqpvmj
yjhamdhh
```
Assigning each of these a symbol, using the preference procedure, we get:


```
aabmevmt aa
abhhwzpo ab
aehwwogz ae
afbvhlke af
afycbvxv ay
agfigxja ag
agxdnjyc ax
....
xoyittxg yi
xrlkgqbe lb
xxutzias zi
ycykczyb yy
ygnoizht yn
yivqpvmj ym
yjhamdhh jm
```
Now, reverse the list of symbols. This starts:


```
jm ym yn yy zi lb yi ...
```
The first 5 symbols on this reversed list (jm, ym, yn, yy, and zi) are in alphabetical order. However, the sixth symbol (lb) comes before the fifth symbol in alphabetical order. Thus my score is 5. How high can you get?


```
jm
```

```
ym
```

```
yn
```

```
yy
```

```
zi
```

```
lb
```
# Verification script
Here is a Python script you can use to make sure your submission is valid and to compute your score.

(http://pastebin.com/yX9hs0We)

----
## **DISCLAIMER**
This prompt has been adapted from [275 [Hard] Splurthian Chemistry 103](https://www.reddit.com/r/dailyprogrammer/comments/4t11c3/20160715_challenge_275_hard_splurthian_chemistry/
)

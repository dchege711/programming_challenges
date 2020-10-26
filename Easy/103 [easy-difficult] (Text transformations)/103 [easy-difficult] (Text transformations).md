---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [103 (easy-difficult) (Text transformations)](https://www.reddit.com/r/dailyprogrammer/comments/11erhd/10132012_challenge_103_easydifficult_text/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

### Easy
Back in the 90s (and early 00s) people thought it was a cool idea to \/\/|2][73 |_1|<3 7H15 to bypass text filters on BBSes. They called it Leet (or 1337), and it quickly became popular all over the internet. The habit has died out, but it's still quite interesting to see the various replacements people came up with when transforming characters.

(http://en.wikipedia.org/wiki/Leet)
Your job's to write a program that translates normal text into Leet, either by hardcoding a number of translations (e.g. A becomes either 4 or /-\, randomly) or allowing the user to specify a random translation table as an input file, like this:


```
A    4 /-\
B    |3 [3 8
C    ( {
(etc.)
```
Each line in the table contains a single character, followed by whitespace, followed by a space-separated list of possible replacements. Characters should have some non-zero chance of not being replaced at all.

### Intermediate
Add a --count option to your program that counts the number of possible outcomes your program could output for a given input. Using the entire translation table from Wikipedia, how many possible results are there for ./leet --count "DAILYPROG"? (Note that each character can also remain unchanged.)


```
--count
```
(http://en.wikipedia.org/wiki/Leet#Orthography)

```
./leet --count "DAILYPROG"
```
Also, write a translation table to convert ASCII characters to hex codes (20 to 7E), i.e. "DAILY" -> "4441494C59".


```
20
```

```
7E
```

```
"DAILY" -> "4441494C59"
```
### Difficult
Add a --decode option to your program, that tries to reverse the process, again by picking any possibility randomly: /\/\/ could decode to M/, or NV, or A/V, etc.


```
--decode
```

```
/\/\/
```

```
M/
```

```
NV
```

```
A/V
```
Extend the --count option to work with --decode: how many interpretations are there for a given input?


```
--count
```

```
--decode
```

----
## **DISCLAIMER**
This prompt has been adapted from [103 [easy-difficult] (Text transformations)](https://www.reddit.com/r/dailyprogrammer/comments/11erhd/10132012_challenge_103_easydifficult_text/
)

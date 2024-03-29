---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [271 (Easy) Critical Hit](https://www.reddit.com/r/dailyprogrammer/comments/4nvrnx/20160613_challenge_271_easy_critical_hit/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Description
Critical hits work a bit differently in this RPG.  If you roll the maximum value on a die, you get to roll the die again and add both dice rolls to get your final score.  Critical hits can stack indefinitely -- a second max value means you get a third roll, and so on.  With enough luck, any number of points is possible.

# Input

```
d
```

```
h
```
# Output
The probability of you getting h or more points with your die.


```
h
```
# Challenge Inputs and Outputs

|Input: d|Input: h|Output|
| --- | --- | --- |
|4|1|1|
| --- | --- | --- |
|4|4|0.25|
| --- | --- | --- |
|4|5|0.25|
| --- | --- | --- |
|4|6|0.1875|
| --- | --- | --- |
|1|10|1|
| --- | --- | --- |
|100|200|0.0001|
| --- | --- | --- |
|8|20|0.009765625|
| --- | --- | --- |
|
```
d
```

```
h
```
# Secret, off-topic math bonus round
What's the expected (mean) value of a D4? (if you are hoping for as high a total as possible).

thanks to /u/voidfunction for submitting this challenge through /r/dailyprogrammer_ideas.

(/u/voidfunction)
(/r/dailyprogrammer_ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [271 [Easy] Critical Hit](https://www.reddit.com/r/dailyprogrammer/comments/4nvrnx/20160613_challenge_271_easy_critical_hit/
)

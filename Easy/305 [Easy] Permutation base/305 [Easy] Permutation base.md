---
draft: true
----

# [305 (Easy) Permutation base](https://www.reddit.com/r/dailyprogrammer/comments/5xu7sz/20170306_challenge_305_easy_permutation_base/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

There may be an actual name to this base system (let us/me know in comments), and there is a math insight that makes this problem even easier, but it is still pretty easy with no math insight.

for "permutation base 2", the indexes and values start with:


|index|value|
| --- | --- |
|0|0|
| --- | --- |
|1|1|
| --- | --- |
|2|00|
| --- | --- |
|3|01|
| --- | --- |
|4|10|
| --- | --- |
|5|11|
| --- | --- |
|6|000|
| --- | --- |
|7|001|
| --- | --- |
|8|010|
| --- | --- |
|9|011|
| --- | --- |
|sample challenge:

what is the base-value for index 54?


```
54
```
what is the index-value for base 111000111


```
111000111
```
solutions:


```
permbase2 54
```
1 1 0 0 0


```
permbase2 inv 1 1 1 0 0 0 1 1 1
```
965

challenge index inputs (some are 64 bit+ inputs)

234234234
234234234234234
234234234234234234234234 

challenge value inputs

000111000111111000111111000111111000111
11111111000111000111111000111111000111111000111  

bonus:

extend the function to work with any base.  Base 10 index value 10 is 00.  index value 109 is 99


```
10
```

```
00
```

```
109
```

```
99
```

----
## **DISCLAIMER**
This prompt has been adapted from [305 [Easy] Permutation base](https://www.reddit.com/r/dailyprogrammer/comments/5xu7sz/20170306_challenge_305_easy_permutation_base/
)

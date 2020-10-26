---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [76 (easy) (Title case)](https://www.reddit.com/r/dailyprogrammer/comments/wjzly/7132012_challenge_76_easy_title_case/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Write a function that transforms a string into title case. This mostly means: capitalizing only every first letter of every word in the string. However, there are some non-obvious exceptions to title case which can't easily be hard-coded. Your function must accept, as a second argument, a set or list of words that should not be capitalized. Furthermore, the first word of every title should always have a capital leter. For example:

(http://en.wikipedia.org/wiki/Letter_case#Headings_and_publication_titles)

```
exceptions = ['jumps', 'the', 'over']
titlecase('the quick brown fox jumps over the lazy dog', exceptions)
```
This should return:


```
The Quick Brown Fox jumps over the Lazy Dog
```
An example from the Wikipedia page:


```
exceptions = ['are', 'is', 'in', 'your', 'my']
titlecase('THE vitamins ARE IN my fresh CALIFORNIA raisins', exceptions)
```
Returns:


```
The Vitamins are in my Fresh California Raisins
```

----
## **DISCLAIMER**
This prompt has been adapted from [76 [easy] (Title case)](https://www.reddit.com/r/dailyprogrammer/comments/wjzly/7132012_challenge_76_easy_title_case/
)

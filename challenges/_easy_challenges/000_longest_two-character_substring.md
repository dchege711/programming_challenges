---

layout: default
title: Longest Two Character Substring
date: 2017-09-02 
type: writeup

---

<nav aria-label="Breadcrumb" class="breadcrumb">
    <ul>
        <li><a href="{{site.baseurl}}/">Home</a></li>
        <li><a href="{{site.baseurl}}/easy_challenges">Easy Challenges</a></li>
        <li><span aria-current="page">{{page.title}}</span></li>
    </ul>
</nav>

## Description

[Link to /r/dailyprogrammer post](https://www.reddit.com/r/dailyprogrammer/comments/1g0tw1/easy_longest_twocharacter_substring/)

This programming challenge is a [classic interview question](http://en.wikipedia.org/wiki/Longest_common_substring_problem) for software engineers: given a string, find the longest sub-string that contains, at most, two characters.

Author: [/u/Regul](https://old.reddit.com/u/Regul)

### Formal Inputs & Outputs

#### Input Description

Through standard console input, you will be given a string to search, which only contains lower-case alphabet letters.

#### Output Description

Simply print the longest sub-string of the given string that contains, at most, two unique characters. If you find multiple sub-strings that match the description, print the last sub-string (furthest to the right).

### Sample Inputs & Outputs

#### Sample Inputs

```plain
abbccc
abcabcabcabccc
qwertyytrewq
```

## Sample Outputs

```plain
bbccc
bccc
tyyt
```

----

## Solution

```python
{% include_relative 000_longest_two-character_substring/LongestSubString00.py %}
```

I wrote this solution 2 years ago. What's changed since then?

- I no longer use camel case in Python
- I don't leave print statements inside functions

I think I wasn't too bad a programmer :-)

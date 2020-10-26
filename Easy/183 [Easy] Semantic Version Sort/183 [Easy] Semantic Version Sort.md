---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [183 (Easy) Semantic Version Sort](https://www.reddit.com/r/dailyprogrammer/comments/2igfj9/10062014_challenge_183_easy_semantic_version_sort/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Easy): Semantic Version Sort
(#EasyIcon)
Semantic Versioning, or Semver as it's known on the streets, is an attempt to standardise the way that software versions are incrementally changed. In the world there are many different pieces of software whose developers have conflicting ideas about how software should be developed. For example, Dwarf Fortress is currently at version 0.40.13, whereas Google Chrome (which has been around for 2 years less than Dwarf Fortress) is currently at version 37.0.2062.124. How can those version numbers even be compared? They both represent around the same progress of development but in totally different ways. Semantic versioning aims to solve this problem by splitting the version string into 3, 4 or 5 parts:

(http://www.bay12games.com/dwarves/)
(https://en.wikipedia.org/wiki/Google_Chrome)

```
<major>.<minor>.<patch>-<label>+<metadata>
```
For the purpose of this challenge, you will be sorting a list of Semantic Versions into chronological order, and you will do it like so:

First, compare the major version.

If they are the same, compare the minor version.

If they are the same, compare the patch version.

If those are all the same, check if the version has a label - ignore the content of the label. A version with a label (prerelease) comes before one without a label (final release.)

Ignore the build metadata.

If the semantic versions are still the same at this point, they can be considered the same version. For the purpose of this challenge we won't attempt to parse the label - but if you're feeling up to you can give it a try!

The full specification for Semantic Versioning can be found here.

(http://semver.org/)
# Formal Inputs and Outputs
## Input Description
You will first be given a number N. You will then be given N more lines, each one with a semantic version.

## Output Description
You will print the versions in chronological order, as described by the rules above.

# Sample Inputs and Outputs
## Sample Input

```
7
2.0.11-alpha
0.1.7+amd64
0.10.7+20141005
2.0.12+i386
1.2.34
2.0.11+i386
20.1.1+i386
```
## Sample Output

```
0.1.7+amd64
0.10.7+20141005
1.2.34
2.0.11-alpha
2.0.11+i386
2.0.12+i386
20.1.1+i386
```
# Tip
If your chosen language supports it, create a SemanticVersion record/structure with the appropriate fields. If your language supports it, overload the comparison (<, >) operators to compare for you.


```
SemanticVersion
```

```
<
```

```
>
```
If your language does not support sorting of data structures by default, you could adjust your solution to the Quicksort challenge to work with this one.

(/r/dailyprogrammer/comments/2ejl4x/)

----
## **DISCLAIMER**
This prompt has been adapted from [183 [Easy] Semantic Version Sort](https://www.reddit.com/r/dailyprogrammer/comments/2igfj9/10062014_challenge_183_easy_semantic_version_sort/
)

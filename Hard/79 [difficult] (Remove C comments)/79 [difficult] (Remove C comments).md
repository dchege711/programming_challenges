---
date: '2020-05-03'
draft: true
inherit_date: true
---

# [79 (difficult) (Remove C comments)](https://www.reddit.com/r/dailyprogrammer/comments/wvg2r/7182012_challenge_79_difficult_remove_c_comments/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

In the C programming language, comments are written in two different ways:


```
/* ... */
```

```
// ...
```
Write a program that removes these comments from an input file, replacing them by a single space character, but also handles strings correctly. Strings are delimited by a " character, and \" is skipped over. For example:


```
"
```

```
\"
```

```
int /* comment */ foo() { }
→ int   foo() { }

  void/*blahblahblah*/bar() { for(;;) } // line comment
→ void bar() { for(;;) }  

  { /*here*/ "but", "/*not here*/ \" /*or here*/" } // strings
→ {   "but", "/*not here*/ \" /*or here*/" }
```

----
## **DISCLAIMER**
This prompt has been adapted from [79 [difficult] (Remove C comments)](https://www.reddit.com/r/dailyprogrammer/comments/wvg2r/7182012_challenge_79_difficult_remove_c_comments/
)

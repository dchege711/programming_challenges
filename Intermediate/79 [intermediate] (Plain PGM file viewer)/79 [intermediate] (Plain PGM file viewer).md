---
draft: true
----

# [79 (intermediate) (Plain PGM file viewer)](https://www.reddit.com/r/dailyprogrammer/comments/wvcv9/7182012_challenge_79_intermediate_plain_pgm_file/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

Write a program that converts a "plain" .pgm file passed from stdin to an ASCII representation easily viewable in a terminal. If you're too lazy to read through the specification, the format should be simple enough to reverse-engineer from an example file:

(http://netpbm.sourceforge.net/doc/pgm.html)

```
.pgm
```

```
P2
# feep.pgm
24 7
15
0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
0  3  3  3  3  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15 15 15 15  0
0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0 15  0
0  3  3  3  0  0  0  7  7  7  0  0  0 11 11 11  0  0  0 15 15 15 15  0
0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0  0  0
0  3  0  0  0  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15  0  0  0  0
0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
```

```
P2
```
Your program should use ASCII symbols to represent different grayscale values. Assuming the text is black on a white background, you could use a gradient like this one:


```
" .:;+=%$#"
```
Converted, the example image would look something like this:


```
....  ;;;;  ====  #### 
 .     ;     =     #  # 
 ...   ;;;   ===   #### 
 .     ;     =     #    
 .     ;;;;  ====  #
```

----
## **DISCLAIMER**
This prompt has been adapted from [79 [intermediate] (Plain PGM file viewer)](https://www.reddit.com/r/dailyprogrammer/comments/wvcv9/7182012_challenge_79_intermediate_plain_pgm_file/
)

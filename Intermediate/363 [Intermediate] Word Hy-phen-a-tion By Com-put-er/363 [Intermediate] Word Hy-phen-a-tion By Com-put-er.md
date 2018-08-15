# [363 (Intermediate) Word Hy-phen-a-tion By Com-put-er](https://old.reddit.com/r/dailyprogrammer/comments/8qxpqd/20180613_challenge_363_intermediate_word/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

# Background
In English and many other languages, long words may be broken onto two lines using a hyphen. You don't see it on the web very often, but it's common in print books and newspapers. However, you can't just break apart a word anywhere. For instance, you can split "programmer" into "pro" and "grammer", or into "program" and "mer", but not "progr" and "ammer".

For today's challenge you'll be given a word and need to add hyphens at every position it's legal to break the word between lines. For instance, given "programmer", you'll return "pro-gram-mer".


```
"programmer"
```

```
"pro-gram-mer"
```
There's no simple algorithm that accurately tells you where a word may be split. The only way to be sure is to look it up in a dictionary. In practice a program that needs to hyphenate words will use an algorithm to cover most cases, and then also keep a small set of exceptions and additional heuristics, depending on how tolerant they are to errors.

# Liang's Algorithm
The most famous such algorithm is Frank Liang's 1982 PhD thesis, developed for the TeX typesetting system. Today's challenge is to implement the basic algorithm without any exceptions or additional heuristics. Again, your output won't match the dictionary perfectly, but it will be mostly correct for most cases.

The algorithm works like this. Download the list of patterns for English here. Each pattern is made of up of letters and one or more digits. When the letters match a substring of a word, the digits are used to assign values to the space between letters where they appears in the pattern. For example, the pattern 4is1s says that when the substring "iss" appears within a word (such as in the word "miss"), the space before the i is assigned a value of 4, and the space between the two s's is assigned a value of 1.

(https://gist.githubusercontent.com/cosmologicon/1e7291714094d71a0e25678316141586/raw/006f7e9093dc7ad72b12ff9f1da649822e56d39d/tex-hyphenation-patterns.txt)

```
4is1s
```

```
"iss"
```

```
i
```

```
s
```
Some patterns contain a dot (.) at the beginning or end. This means that the pattern must appear at the beginning or end of the word, respectively. For example, the pattern ol5id. matches the word "solid", but not the word "solidify".


```
.
```

```
ol5id.
```
Multiple patterns may match the same space. In this case the ultimate value of that space is the highest value of any pattern that matches it. For example, the patterns 1mo and 4mok both match the space before the m in smoke. The first one would assign it a value of 1 and the second a value of 4, so this space gets assigned a value of 4.


```
1mo
```

```
4mok
```

```
m
```

```
smoke
```
Finally, the hyphens are placed in each space where the assigned value is odd (1, 3, 5, etc.). However, hyphens are never placed at the beginning or end of a word.

# Detailed example
There are 10 patterns that match the word mistranslate, and they give values for eight different spaces between words. For each of the eight spaces you take the largest value: 2, 1, 4, 2, 2, 3, 2, and 4. The ones that have odd values (1 and 3) receive hyphens, so the result for mistranslate is mis-trans-late.


```
mistranslate
```

```
mistranslate
```

```
mis-trans-late
```

```
m i s t r a n s l a t e
           2               a2n
     1                     .mis1
 2                         m2is
           2 1 2           2n1s2
             2             n2sl
               1 2         s1l2
               3           s3lat
       4                   st4r
                   4       4te.
     1                     1tra
m2i s1t4r a2n2s3l2a4t e
m i s-t r a n s-l a t e
```
# Additional examples

```
mistranslate => mis-trans-late
alphabetical => al-pha-bet-i-cal
bewildering => be-wil-der-ing
buttons => but-ton-s
ceremony => cer-e-mo-ny
hovercraft => hov-er-craft
lexicographically => lex-i-co-graph-i-cal-ly
programmer => pro-gram-mer
recursion => re-cur-sion
```
# Optional bonus
Make a solution that's able to hyphenate many words quickly. Essentially you want to avoid comparing every word to every pattern. The best common way is to load the patterns into a prefix trie, and walk the tree starting from each letter in the word.

It should be possible to hyphenate every word in the enable1 word list in well under a minute, depending on your programming language of choice. (My python solution takes 15 seconds, but there's no exact time you should aim for.)

(https://norvig.com/ngrams/enable1.txt)
Check your solution if you want to claim this bonus. The number of words to which you add 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9 hyphens should be (EDITED): 21829, 56850, 50452, 26630, 11751, 4044, 1038, 195, 30, and 1.


----
## **DISCLAIMER**
This prompt has been adapted from [363 [Intermediate] Word Hy-phen-a-tion By Com-put-er](https://old.reddit.com/r/dailyprogrammer/comments/8qxpqd/20180613_challenge_363_intermediate_word/
)

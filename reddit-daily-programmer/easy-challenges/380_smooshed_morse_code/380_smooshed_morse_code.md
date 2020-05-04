---
title: "380. Smooshed Morse Code"
weight: 380
date: "2019-09-22"
---

## Description

Covert a string, e.g. `programmer` into Morse code, e.g. `.--..-.-----..-..-----..-.` [/r/dailyprogrammer thread](https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/)

## Solution

{{< readfile
    file="content/computer-science/programming-challenges/reddit-daily-programmer/easy-challenges/380_smooshed_morse_code/_380_smooshed_morse_code.py"
    highlight="python">}}

## Bonus Challenges

### Bonus Challenge #1

> The sequence `-...-....-.--.` is the code for four different words (`needing`, `nervate`, `niding`, `tiling`). Find the only sequence that's the code for 13 different words.

{{< readfile
    file="content/computer-science/programming-challenges/reddit-daily-programmer/easy-challenges/380_smooshed_morse_code/challenge_1.py"
    highlight="python">}}

My solution uses a prefix tree to use less memory(? assuming that a significant number of smorse representations share prefixes; after all, the alphabet has 2 characters, so why not). I didn't learn about prefix trees until my second CS class. Using a hash table could work as well, and that's not too involving.

### Bonus Challenge #2

> `autotomous` encodes to `.-..--------------..-...`, which has 14 dashes in a row. Find the only word that has 15 dashes in a row.

{{< readfile
    file="content/computer-science/programming-challenges/reddit-daily-programmer/easy-challenges/380_smooshed_morse_code/challenge_2.py"
    highlight="python">}}

Using python is cheating at this point :-)

### Bonus Challenge #3

> Call a word perfectly balanced if its code has the same number of dots as dashes. `counterdemonstrations` is one of two 21-letter words that's perfectly balanced. Find the other one.

{{< readfile
    file="content/computer-science/programming-challenges/reddit-daily-programmer/easy-challenges/380_smooshed_morse_code/challenge_3.py"
    highlight="python">}}

Neat challenge. The most convenient properties were our alphabet having only two letters and the question being a decision query, e.g. is `word` perfectly balanced? These two qualities made an arithmetic approach feasible.

### Bonus Challenge #4

> `protectorate` is 12 letters long and encodes to `.--..-.----.-.-.----.-..--.`, which is a palindrome (i.e. the string is the same when reversed). Find the only 13-letter word that encodes to a palindrome.

{{< readfile
    file="content/computer-science/programming-challenges/reddit-daily-programmer/easy-challenges/380_smooshed_morse_code/challenge_4.py"
    highlight="python">}}

Ah, the good old palindrome. If I had a shilling for every time an interviewer asked me this question...

### Bonus Challenge #5

> `--.---.---.--` is one of five 13-character sequences that does not appear in the encoding of any word. Find the other four.

{{< readfile
    file="content/computer-science/programming-challenges/reddit-daily-programmer/easy-challenges/380_smooshed_morse_code/challenge_5.py"
    highlight="python">}}

Not trivial, at least to me. The idea is intuitive: have a list of 13 character sequences and check if they appear in any decoding. Finding 13-character sequences is the coin change problem by another name. [I tried to solve it efficiently some time back but failed](https://cards.c13u.com/browse/?cardID=5bb248d57c2b5b00046da685). Chuck bless [itertools.product](https://docs.python.org/3.7/library/itertools.html#itertools.product)

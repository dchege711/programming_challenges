---

layout: default
title: Smooshed Morse Code
date: 2019-09-22
type: writeup

---

<nav aria-label="Breadcrumb" class="breadcrumb">
    <ul>
        <li><a href="/">Home</a></li>
        <li><a href="/easy_challenges">Easy Challenges</a></li>
        <li><span aria-current="page">{{page.title}}</span></li>
    </ul>
</nav>

## Main Challenge

[Link to Reddit Post](https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/)

For the purpose of this challenge, Morse code represents every letter as a sequence of 1-4 characters, each of which is either `.` (dot) or `-` (dash). The code for the letter `a` is `.-`, for `b` is `-...`, etc. The codes for each letter `a` through `z` are:

`.- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --..`

Normally, you would indicate where one letter ends and the next begins, for instance with a space between the letters' codes, but for this challenge, just smoosh all the coded letters together into a single string consisting of only dashes and dots.

### Examples

```shell
smorse("sos") => "...---..."
smorse("daily") => "-...-...-..-.--"
smorse("programmer") => ".--..-.-----..-..-----..-."
smorse("bits") => "-.....-..."
smorse("three") => "-.....-..."
```

An obvious problem with this system is that decoding is ambiguous. For instance, both `bits` and `three` encode to the same string, so you can't tell which one you would decode to without more information.

#### Solution

```python
{% include_relative 380_smooshed_morse_code/_380_smooshed_morse_code.py %}
```

Seemed reasonable for a beginner's challenge.

---

## Bonus Challenges

For these challenges, use the [enable1 word list](https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt). It contains 172,823 words. If you encode them all, you would get a total of 2,499,157 dots and 1,565,081 dashes.

### Bonus Challenge #1

> The sequence `-...-....-.--.` is the code for four different words (`needing`, `nervate`, `niding`, `tiling`). Find the only sequence that's the code for 13 different words.

```python
{% include_relative 380_smooshed_morse_code/challenge_1.py %}
```

My solution uses a prefix tree to use less memory(? assuming that a significant number of smorse representations share prefixes; after all, the alphabet has 2 characters, so why not). I didn't learn about prefix trees until my second CS class. Using a hash table could work as well, and that's not too involving.

### Bonus Challenge #2

> `autotomous` encodes to `.-..--------------..-...`, which has 14 dashes in a row. Find the only word that has 15 dashes in a row.

```python
{% include_relative 380_smooshed_morse_code/challenge_2.py %}
```

Using python is cheating at this point :-)

### Bonus Challenge #3

> Call a word perfectly balanced if its code has the same number of dots as dashes. `counterdemonstrations` is one of two 21-letter words that's perfectly balanced. Find the other one.

```python
{% include_relative 380_smooshed_morse_code/challenge_3.py %}
```

Neat challenge. The most convenient properties were our alphabet having only two letters and the question being a decision query, e.g. is `word` perfectly balanced? These two qualities made an arithmetic approach feasible.

### Bonus Challenge #4

> `protectorate` is 12 letters long and encodes to `.--..-.----.-.-.----.-..--.`, which is a palindrome (i.e. the string is the same when reversed). Find the only 13-letter word that encodes to a palindrome.

```python
{% include_relative 380_smooshed_morse_code/challenge_4.py %}
```

Ah, the good old palindrome. If I had a shilling for every time an interviewer asked me this question...

### Bonus Challenge #5

> `--.---.---.--` is one of five 13-character sequences that does not appear in the encoding of any word. Find the other four.

```python
{% include_relative 380_smooshed_morse_code/challenge_5.py %}
```

Not trivial, at least to me. The idea is intuitive: have a list of 13 character sequences and check if they appear in any decoding. Finding 13-character sequences is the coin change problem by another name. [I tried to solve it efficiently some time back but failed](https://cards.c13u.com/browse/?cardID=5bb248d57c2b5b00046da685). Chuck bless [itertools.product](https://docs.python.org/3.7/library/itertools.html#itertools.product)

---

Thanks to [u/Separate_Memory](https://www.reddit.com/u/Separate_Memory) for inspiring this challenge on [r/dailyprogrammer_ideas](https://www.reddit.com/r/dailyprogrammer_ideas)!

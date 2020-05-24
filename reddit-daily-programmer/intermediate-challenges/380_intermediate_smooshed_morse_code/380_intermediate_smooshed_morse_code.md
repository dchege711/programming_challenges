---

layout: writeup
title: Smooshed Morse Code (Intermediate)
date: 2019-10-07
type: writeup

---

## Description

[Link to original Reddit submission](https://www.reddit.com/r/dailyprogrammer/comments/cn6gz5/20190807_challenge_380_intermediate_smooshed/)

The challenge is to match a string like `.--...-.-.-.....-.--........----.-.-..---.---.--.--.-.-....-..-...-.---..--.----..` to a permutation of the English alphabet that would produce it, e.g. `wirnbfzehatqlojpgcvusyxkmd`. Note that there may be more than one valid permutation.

## Solution

I'm used to problems where the resources are not depleted, e.g. I can use `a` as many times as I want. The trickiest part was keeping track of the substring indexes.

{{< readfile file="content/computer-science/programming-challenges/reddit-daily-programmer/intermediate-challenges/380_intermediate_smooshed_morse_code/smooshed_morse_decoder.py" highlight="python" >}}

## Bonus 1: How Fast Can You Go?

For [this list of a 1000 inputs](https://gist.github.com/cosmologicon/415be8987a24a3abd07ba1dddc3cf389#file-smorse2-bonus1-in), the above program takes 33 minutes. Comparing this runtime with other submissions is tricky because the task was to find _any_ consistent decoding. My solution finds all possible decodings.

## Bonus 2: Find the least ambiguous encoding

Find the the smooshed morse code that has the fewest number of matching permutations. String to beat: `......-..--...---.-....---...--....--.-..---.....---.-.---..---.-....--.-.---.-.--` (41 decodings)

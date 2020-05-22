---

layout: writeup
title: Smooshed Morse Code (Intermediate)
date: 2019-10-07
type: writeup
draft: true

---

## Description

[Link to original Reddit submission](https://www.reddit.com/r/dailyprogrammer/comments/cn6gz5/20190807_challenge_380_intermediate_smooshed/)

The challenge is to match a string like `.--...-.-.-.....-.--........----.-.-..---.---.--.--.-.-....-..-...-.---..--.----..` to all permutations of the English alphabet that would produce it, e.g. `wirnbfzehatqlojpgcvusyxkmd`.

## My Solution

I'm used to problems where the resources are not depleted, e.g. I can use `a` as many times as I want. The trickiest part was keeping track of the substring indexes.

{{< readfile file="content/computer-science/programming-challenges/reddit-daily-programmer/intermediate-challenges/380_intermediate_smooshed_morse_code/_380__smooshed_morse_code.py" highlight="python" >}}

## Bonus 1

For [this list of a 1000 inputs](https://gist.github.com/cosmologicon/415be8987a24a3abd07ba1dddc3cf389#file-smorse2-bonus1-in), the above program takes 1,983 seconds. [/u/Gprime5's solution](https://www.reddit.com/r/dailyprogrammer/comments/cn6gz5/20190807_challenge_380_intermediate_smooshed/ew838rx/) runs in 69 seconds. I think it comes down to DFS versus BFS. They stop at the first decoding for the provided smooshed input, while I find *all* possible decodings of the input.

That said, I'm not enthusiastic enough to reimplement my solution as a DFS for an apples-to-apples comparison. The first 10 inputs have an average of 2,343 consistent decodings. Going by /u/Gprim5's runtime, probably within \\(69/1983 \approx 3.46\\%\\) of the search space explored by BFS.

## Bonus 2

Find the the smooshed morse code that has the fewest number of matching permutations. String to beat: `......-..--...---.-....---...--....--.-..---.....---.-.---..---.-....--.-.---.-.--` (41 decodings)

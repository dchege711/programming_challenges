---
title: "139. Telephone Keypads"
draft: true
weight: 139
---

## Description

[/r/dailyprogrammer post](https://www.reddit.com/r/dailyprogrammer/comments/1sody4/12113_challenge_139_intermediate_telephone_keypads/)

Given a series of digits from a telephone keypad, and a list of English words, print the word(s) that fit the starting pattern.

For example, given `7777 666 555 3`, we map

* `7777` => `S`
* `666` => `O`
* `555` => `L`
* `3` => `D`

Giving us: `sold, solder, soldered, soldering, ...`.

Extra challenge: Make efficient predictions without knowing the number of presses, e.g. `7 6 5 3` could still mean `sold`.

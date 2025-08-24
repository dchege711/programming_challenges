---
date: 2025-07-02
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/03-null-it-over/03-mull-it-over/
title: 'AoC 2024 Day 03: Mull It Over'
---

## Problem Statement

### Part One

The computer appears to be trying to run a program, but its memory is
**corrupted**. All of the instructions have been jumbled up!

It seems like the goal of the program is just to **multiply some numbers**. It
does that with instructions like `mul(X,Y)` where `X` and `Y` are each 1-3
digit numbers. For instance, `mul(44,46)` multiplies `44` and `46` to get a
result of `2024`.

However, because the program's memory has been corrupted, there are also invalid
characters that should be **ignored**, even if they look like a `mul`
instruction. Sequences like `mul(4*`, `mul(6,9!`, `?(12,34)`, or `mul ( 2 , 4 )`
do **nothing**.

For example, consider the following section of corrupted memory:

x**mul(2,4)**%&mul[3,7]!@^do\_not\_**mul(5,5)**+mul(32,64]then(**mul(11,8)mul(8,5)**)

Only the four highlighted sections are real `mul` instructions. Adding up the
result of each instruction produces \\(2 \times 4 + 5 \times 5 + 11 \times 8 + 8
\times 5 = 161\\).

Scan the corrupted memory for uncorrupted `mul` instructions. **What do you get
if you add up all of the results of the multiplications?**

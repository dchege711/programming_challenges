---
date: '2020-05-03'
draft: true
inherit_date: true
title: 217. Lumberjack Pile Problem
weight: 217
---

## Description

Given an \\(n \times n \\) grid, with each slot possibly containing some number of logs, distribute the remaining \\(k\\) logs as evenly as possible, without moving the already piled logs. [/r/dailyprogrammer link](https://www.reddit.com/r/dailyprogrammer/comments/3840rp/20150601_challenge_217_easy_lumberjack_pile/)

For instance, adding \\(7\\) more logs gives us:

$$
\begin{bmatrix}
    1 & 1 & 1 \\\\
    2 & 1 & 3 \\\\
    1 & 4 & 1 \\\\
\end{bmatrix} \implies \begin{bmatrix}
    1^{+2} & 1^{+1} & 1^{+1} \\\\
    2^{+0} & 1^{+1} & 3^{+0} \\\\
    1^{+1} & 4^{+0} & 1^{+1} \\\\
\end{bmatrix} \implies \begin{bmatrix}
    3 & 2 & 2 \\\\
    2 & 2 & 3 \\\\
    2 & 4 & 2 \\\\
\end{bmatrix}
$$

---
cited-authors:
- Wastl, Eric
date: 2025-12-07
domains:
- adventofcode.com
title: 'AoC 2024 Day 13: Claw Contraption'
---

## Parsing

The input is a list of machine configurations, where buttons \\(A\\) and \\(B\\)
move the claw some distance \\(X\\) and \\(Y\\), and the location of the prize
is specified. {{% cite AoC2024Day13 %}}

```txt
Button A: X+43, Y+68
Button B: X+10, Y+36
Prize: X=4800, Y=6250

Button A: X+63, Y+41
Button B: X+89, Y+18
Prize: X=17648, Y=19276
```

## Part One

It costs \\(3\\) tokens to push the \\(A\\) button and \\(1\\) token to push the
\\(B\\) button. What is the fewest tokens you would have to spend to win all
possible prizes? {{% cite AoC2024Day13 %}}

## References

1. {{< citation
  id="AoC2024Day13"
  author="Eric Wastl"
  title="Day 13 - Advent of Code 2024: Claw Contraption"
  url="https://adventofcode.com/2024/day/13"
  accessed="2025-12-07" >}}

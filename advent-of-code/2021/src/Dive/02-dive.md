---
date: 2022-02-19
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/02-dive/02-dive/
title: 02. Dive!
weight: 2
---

{{< citation
  id="AoC2021-02"
  title="Day 2 - Advent of Code 2021"
  url="https://adventofcode.com/2021/day/2"
  accessed="2022-02-19" >}}

## Problem Statement

### Part One

Now, you need to figure out how to pilot this thing.

It seems like the submarine can take a series of commands like `forward
1`, `down 2`, or `up 3`:

* `forward X` increases the horizontal position by `X` units.
* `down X` **increases** the depth by `X` units.
* `up X` **decreases** the depth by `X` units.

{{% comment %}}

Can't submarines move backwards? Why is there no `backward X`?

{{% /comment %}}

Note that since you're on a submarine, `down` and `up` affect your
**depth**, and so they have the opposite result of what you might
expect.

The submarine seems to already have a planned course (your puzzle
input). You should probably figure out where it's going. For example:

```md
forward 5
down 5
forward 8
up 3
down 8
forward 2
```

Your horizontal position and depth both start at `0`. The steps above
would then modify them as follows:

- `forward 5` adds `5` to your horizontal position, a total of `5`.
- `down 5` adds `5` to your depth, resulting in a value of `5`.
- `forward 8` adds `8` to your horizontal position, a total of `13`.
- `up 3` decreases your depth by `3`, resulting in a value of `2`.
- `down 8` adds `8` to your depth, resulting in a value of `10`.
- `forward 2` adds `2` to your horizontal position, a total of `15`.

After following these instructions, you would have a horizontal position
of `15` and a depth of `10`. (Multiplying these together produces
`150`.)

Calculate the horizontal position and depth you would have after the
following [planned course](https://adventofcode.com/2021/day/2/input).
**What do you get if you multiply your final horizontal position by your
final depth?**

{{% comment %}}

Not sure why we're interested in \\(h \cdot d\\) rather than in
\\(\sqrt{h^2 + d^2}\\), which represents the distance. Part II might
have this.

Another extension would be taking into account the effects of water
current. If a submarine is not actively moving, it seems like it should
drift depending on where the currents' magnitude and direction.

{{% /comment %}}

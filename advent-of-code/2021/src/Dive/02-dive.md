---
cited-authors:
- Wastl, Eric
date: 2022-02-19
domains:
- adventofcode.com
- jhidding.github.io
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/Dive/02-dive/
title: 'AoC 2021 Day 02: Dive!'
weight: 2
---

{{< citation
  id="AoC2021-02"
  title="Day 2 - Advent of Code 2021"
  url="https://adventofcode.com/2021/day/2"
  author="Eric Wastl"
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

Calculate the horizontal position and depth you would have after the
following [planned course](https://adventofcode.com/2021/day/2/input).
**What do you get if you multiply your final horizontal position by your
final depth?**

{{% comment %}}

Not sure why we're interested in \\(h \cdot d\\) rather than in
\\(\sqrt{h^2 + d^2}\\), which represents the distance. Part II might
have this. Update: Part II doesn't address this.

Another extension would be taking into account the effects of water
current. If a submarine is not actively moving, it seems like it should
drift depending on where the currents' magnitude and direction. Update:
Part II doesn't address this.

{{% /comment %}}

### Part Two

Based on your calculations, the planned course doesn't seem to make any
sense. You find the submarine manual and discover that the process is
actually slightly more complicated.

In addition to horizontal position and depth, you'll also need to track
a third value, **aim**, which also starts at `0`. The commands also mean
something entirely different than you first thought:

* `down X` **increases** your aim by `X` units.
* `up X` **decreases** your aim by `X` units.
* `forward X` does two things:
  * It increases your horizontal position by `X` units.
  * It increases your depth by your aim **multiplied by** `X`.

Again note that since you're on a submarine, `down` and `up` do the
opposite of what you might expect: "down" means aiming in the positive
direction.

Using this new interpretation of the commands, calculate the horizontal
position and depth you would have after following the planned course.
**What do you get if you multiply your final horizontal position by your
final depth?**

## My Solution

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/src/Dive/Dive.hs"
  highlight="haskell"
  id="Dive.hs">}}

## Learning from Others' Solutions

Once again, I find myself admiring {{% cite HiddingAoC2021-02 %}}'s
solution, which has fewer moving parts.

{{% comment %}}

[Remarks on how Hidding parses the file into `[Instruction]`]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/haskell-meta#HiddingAoC2021-02-ParsingInput" >}}).

{{% /comment %}}

```hs
data Instruction = GoForward Int | GoUp Int | GoDown Int deriving (Show)
type Pos = (Int, Int)

moveA :: Pos -> Instruction -> Pos
moveA (x, y) (GoForward dx) = (x + dx, y)
moveA (x, y) (GoUp dy)      = (x, y + dy)
moveA (x, y) (GoDown dy)    = (x, y - dy)

productOfFinalPosition :: [Instruction] -> Int
productOfFinalPosition instructions = x * y
  where (x, y) = foldl moveA (0, 0) instructions
```

... compared to mine:

```hs
data DiveDirection = Up | Down | Forward deriving (Eq, Show)
directionAndMagnitude :: String -> Maybe (DiveDirection, Int)
applySign :: (DiveDirection, Int) -> Int
isForward :: (DiveDirection, Int) -> Bool

-- The fact that I'm repeatedly using `(DiveDirection, Int)` is a sign
-- that something is wrong with my abstraction that only considers
-- `DiveDirection`. My major thought at the time was that, "Convert
-- string inputs to enum values at parsing time," and once I achieved
-- that, I patted myself on the back for applying a best practice. I did
-- not stop to consider what else I could do to improve the abstraction.

productOfFinalPosition :: [String] -> Int
productOfFinalPosition steps =
  let parsedSteps = mapMaybe directionAndMagnitude steps
      finalHorizontalPos = sum $ map applySign $ filter isForward parsedSteps
      finalVerticalPos = sum $ map applySign $ filter (not . isForward) parsedSteps
  in finalHorizontalPos * finalVerticalPos

-- Granted, during this time, I hadn't known about folding.
```

... really shows the code clarity that can be obtained from creating the
proper abstractions.

{{% comment %}}

{{< figure
  src="/img/computer-science/programming-challenges/advent-of-code/2021/stack-more-functions.jpg"

  caption=`"sum $ map applySign $ filter (not . isForward) parsedSteps",
  colorized, Feb 2022.`>}}

{{% /comment %}}

{{% comment %}}

Copilot suggested `product . foldl' moveA (0, 0)`, but `product (3, 3) =
3` while `product [3, 3] = 9`. Color me surprised.

{{% /comment %}}

{{% cite HiddingAoC2021-02 %}}'s solution for part two contains syntax
that I am not familiar with:

```hs
-- `Navigation` is using the record syntax. The compiler will generate
-- accessor functions and update functions for the fields. Note that the
-- records are still immutable, so the update syntax `y { x = z}`
-- creates a new independent value. Also note that the compiler does not
-- enforce that all fields are set. Accessing unspecified fields results
-- in a runtime error. [1]
--
-- [1]: https://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_(Record_Syntax)
data Navigation = Navigation {depth :: Int, aim :: Int, x :: Int}

moveB :: Navigation -> Instruction -> Navigation
-- The `..` syntax is enabled by the RecordWildCards GHC language
-- extension. Each elided field `f` is replaced by the pattern `f = f`.
-- [1].
--
-- The `@` syntax that is preceded by whitespace is enabled by the
-- TypeApplications GHC language extension [2].
--
-- When the `@` is not preceded by whitespace, it is being
-- used as an as-pattern, which names a pattern for use of the RHS of an
-- equation, e.g. `f s@(x:xs) = x:s` [3].
--
-- [1]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html
-- [2]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html
-- [3]: https://www.haskell.org/tutorial/patterns.html
moveB n@Navigation{ .. } (GoForward dx) = n{ x = x + dx
                                           , depth = depth + aim * dx}
moveB n@Navigation{ .. } (GoUp dAim)      = n{ aim = aim - dAim }
moveB n@Navigation{ .. } (GoDown dAim)    = n{ aim = aim + dAim }

productOfFinalPositionWithNewIntepretation :: [Instruction] -> Int
productOfFinalPositionWithNewIntepretation instructions = x * depth
  where Navigation{ .. } = foldl moveB (Navigation 0 0 0) instructions
```

The fact that we can do `n{x = x + dx}` confused me because I didn't
think `x = x + dx` is valid Haskell syntax, given it is not a generally
true mathematical equation. However, the first `x` refers to the `x`
field in `n`, while the second `x` refers to the one that was
autogenerated by the `RecordWildCards` extension.

{{% open-comment %}}

[Le's
solution](https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day02.md)
starts with, "Day 2 has a satisfying "unified" solution for both parts
that can be derived from group theory!" and I'm not about that pure math
life right now. ┐( ˘ ､ ˘ )┌

It does mention an #adventofcode channel in libera IRC, and that's
something worth checking out and possibly joining.

{{% /open-comment %}}

## References

1. {{< citation
  id="HiddingAoC2021-02"
  title="Advent of Code 2021: Day 2: Dive!"
  url="https://jhidding.github.io/aoc2021/#day-2-dive"
  accessed="2022-02-22" >}}

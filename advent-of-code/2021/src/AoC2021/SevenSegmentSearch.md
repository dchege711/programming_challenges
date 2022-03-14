---
affiliations:
- Columbia University
- Semantic Designs
authors:
- Gill, Andy
- Morrison, Donald R
- Okasaki, Chris
date: 2022-03-07
domains:
- adventofcode.com
- github.com
- gitlab.haskell.org
- hackage.haskell.org
- haskell-containers.readthedocs.io
- ittc.ku.edu
- scholar.google.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/AoC2021/SevenSegmentSearch/
publications:
- Journal of the ACM
- Workshop on ML
title: 'AoC 2021 Day 08: Seven Segment Search'
weight: 8
---

{{< citation
    id="AoC2021-08"
    title="Day 8 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/8"
    accessed="2022-03-07" >}}

## Part I Description

*You barely reach the safety of the cave when the whale smashes into the cave
mouth, collapsing it. Sensors indicate another exit to this cave at a much
greater depth, so you have no choice but to press on.*

*As your submarine slowly makes its way through the cave system, you notice that
the four-digit seven-segment displays in your submarine are malfunctioning;
they must have been damaged during the escape. You'll be in a lot of trouble
without them, so you'd better figure out what's wrong.*

*Each digit of a seven-segment display is rendered by turning on or off any of
the seven segments named `a` through `g`:*

```md
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
```

*So, to render a `1`, only segments `c` and `f` would be turned on; the rest
would be off. To render a `7`, only segments `a`, `c`, and `f` would be turned
on.*

*The problem is that the signals which control the segments have been mixed up
on each display. The submarine is still trying to display numbers by producing
output on signal wires `a` through `g`, but those wires are connected to
segments randomly. Worse, the wire/segment connections are mixed un separately
for each four-digit display! (All of the digits within a display use the same
connections, though.)*

*So, you might know that only signal wires `b` and `g` are turned on, but that
doesn't mean segments `b` and `g` are turned on: the only digit that uses two
segments is `1`, so it must mean segments `c` and `f` are meant to be on. With
just that information, you still can't tell which wire (b/g) goes to which
segment (c/f). For that, you'll need to collect more information.*

*For each display, you watch the changing signals for a while, make a note of
all ten unique signal patterns you see, and then write down a single four-digit
output value (your puzzle input). Using the signal patterns, you should be able
to work out which patterns corresponds to which digit.*

*For example, here's what you might see in a single entry in your notes:*

```txt
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```

*Each entry consists of ten unique signal patterns, a `|` delimiter, and
finally the four-digit output value. Within an entry, the same wire/segment
connections are used (but you don't know what the connections actually are). The
unique signal patterns correspond to the ten different ways the submarine tries
to render a digit using the current wire/segment connections. Because `7` is the
only digit that uses three segments, `dab` in the above example means that to
render `7`, signal lines `d`, `a`, and `b` are on. Because `4` is the only digit
that uses four segments, `eafb` means that to render a `4`, signal lines `e`,
`a`, `f`, and `b` are on.*

*Using this information, you should be able to work out which combination of
signal wires corresponds to each of the ten digits. Then, you can decode the
four digit output value. Unfortunately, in the above example, all of the digits
in the output value `cdfeb fcadb cdfeb cdbaf` use five segments and are more
difficult to deduce.*

*For now, focus on the easy digits. Because the digits `1`, `4`, `7`, and `8`
each use a unique number of segments, you should be able to tell which
combinations of signals correspond to those digits.*

***In the output values (the part after the `|` on each line), how many times
do digits `1`, `4`, `7`, or `8` appear?***

## Input Representation

The line `be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe
cefdb cefbgd gcbe` has `cfbegad` matching with `fdgacbe` in the output value, so
I need a representation that allows those two to be linked. Sorting the
characters is sufficient as it gives `abcdefg` in both cases.

The ten signal patterns are in no particular order, so a `[String]` will do. The
output values do not need to be in any particular order, so a `[String]` will
also do.

```hs
{-#  OPTIONS_GHC -Wall  #-}

module AoC2021.SevenSegmentSearch
    (
        SevenSegmentDisplay(..),
        numOf1478AppearancesInOutput,
        sumOfOutputValues
    )
where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

data SevenSegmentDisplay = SevenSegmentDisplay{
    uniquePatterns :: [String], outputValues :: [String]} deriving Show
```

## Part I Solution

```hs
numActiveSegmentsToDigits :: IntMap.IntMap [Int]
numActiveSegmentsToDigits = IntMap.fromList
    [(6, [0, 6, 9]), (2, [1]), (5, [2, 3, 5]), (4, [4]), (3, [7]), (7, [8])]

nonAmbiguousLengths :: IntSet.IntSet
--  There is also `Map.keysSet` but that returns an
nonAmbiguousLengths = IntSet.fromList $ IntMap.keys $
    IntMap.filter (\t -> length t == 1) numActiveSegmentsToDigits
```

The `containers` package provides `IntMap` and `IntSet` in addition to the
general `Map` and `Set` data structures. {{% cite containersHaskell %}} This
distinction is motivated by {{% cite Okasaki1998 %}}'s work on finite maps that
are based on {{% cite Morrison1968 %}}'s Patricia trees, instead of the usual
base of balanced binary search trees. While both bases have fast lookups and
inserts, Patricia trees have fast merges of two containers. {{% cite
Okasaki1998 %}}

{{% comment %}}

I've been getting the vibe that Haskell is more explicit in its connection to
academia, e.g. foundational papers being linked from API docs, and library
writers and maintainers being faculty in CS departments.

{{% /comment %}}

```hs
numOf1478AppearancesInOutput :: [SevenSegmentDisplay] -> Int
numOf1478AppearancesInOutput = foldr f 0 where
    f :: SevenSegmentDisplay -> Int -> Int
    f SevenSegmentDisplay{ outputValues=outputValues } prevSum =
        prevSum + length (
            filter (\s -> IntSet.member (length s) nonAmbiguousLengths)
            outputValues)
```

Pattern-matching using `SevenSegmentDisplay{ outputValues=outputValues }` leads
to a `Wname-shadowing` HLint warning on the second `outputValues`. {{% cite
LeventErkok2020 %}} notes that either of the `NamedFieldPuns` or
`RecordWildCards` extensions allows shadowing of field names. However, adding
either language extension results in HLint warning that the `LANGUAGE` pragma is
unused.

{{% comment %}}

Compared to other Part I's, this one felt too straightforward. Most of the
difficulty was in using `parsec` to parse the input line.

{{% /comment %}}

## Part II Description

{{% priors %}}

Part II might feature additional information to distinguish additional digits.
Maybe the output values follow some pattern, e.g. the digits are always
increasing from right to left, not all digits are possible for a given output,
etc.

Update: I was wrong. We *do* have enough information to deduce all of the
digits. I don't see how this is always possible.

{{% /priors %}}

*Through a little deduction, you should now be able to determine the remaining
digits. Consider again the first example above:*

```txt
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```

*After some careful analysis, the mapping between signal wires and segments only
make sense in the following configuration:*

```md
 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
```

*So, the unique signal patterns would correspond to the following digits:*

```md
acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1
```

*Then, the four digits of the output value can be decoded:*

```md
cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3
```

*Therefore, the output value for this entry is `5353`.*

*For each entry, determine all of the wire/segment connections and decode the
four-digit output values. **What do you get if you add up all of the output
values?***

## Part II Solution

In the non-jumbled up case, the matching of digits to segments is:

```md
1 -   c  f
4 -  bcd f
7 - a c  f
8 - abcdefg

0 - abc efg
2 - a cde g
3 - a cd fg
5 - ab d fg
6 - ab defg
9 - abcd fg
```

From Part I, the jumbled representations of `1`, `4`, `7`, and `8` can be
identified by counting the number of segments. We can go further and note that
there are shared segments, for example, `0` and `1` both have segments `c` and
`f`. So, solving Part II comes down to building up from the base provided by
`1`, `4`, `7`, and `8`.

The union of the active segments in `147` is `abcdf`, but that doesn't seem
helpful. `8` isn't helpful because it has all of the segments active. The union
of `1x`'s segments (for \\(x \in [4, 7, 8]\\)) doesn't seem helpful because it
will be the same as the active segments for \\(x\\). The union of `47`'s active
segments is `abcdf`, which also doesn't correspond to a digit.

If we go one level deeper, we might deduce an additional digit. With regard to
`x`'s active segments,  \\(x \in [1, 4, 7, 8]\\), being a subset of another
number `y`'s active segments, \\(y \in [0, 2, 3, 5, 6, 9]\\):

```md
1: 0, 3, 9
4: 9
7: 0, 3, 9
```

Nothing jumps out yet. Maybe looking at `x`s and `y`s that share segments might
be illuminating? `1`'s `cf` shares at least one active segment with all `y`s
and therefore given that the union of `1x`'s active segments is the same as that
of `x`, proceeding further doesn't seem useful. Maybe looking at non-shared
segments between `x`s and `y`s helps? Nah, that wouldn't provide additional
info than what I got from the shared segments analysis.

Maybe I can do something with the `y`s:

```md
0 - abc efg (6)
6 - ab defg (6)
9 - abcd fg (6)

2 - a cde g (5)
3 - a cd fg (5)
5 - ab d fg (5)
```

The union of `069`'s active segments is `abcdefg`, and so is the union of
`235`'s active segments, so that's not helpful.

The intersection of `069`'s active segments is `abfg`, and the complement of
this intersection is `cde`; nothing useful yet.

{{% comment %}}

I'm mostly talking in set terminology, so maybe instead of using `String` to
represent a display digit, I should have used a `Set Char`.

{{% /comment %}}

The intersection of `253`'s active segments is `adg`, and the complement of this
intersection is `bcef`; nothing useful yet.

The union of the previous two intersections (`abfg` and `adg`) is `abdfg`, and
for once we have something useful as that equals to `5`'s active segments! The
complement of `abdfg` is `ce`, but that's not useful.

Of `23`'s active segments, the union is `acdefg`, the complement of the union is
`b`, the intersection is `acdg`, and the complement of this intersection is
`bef`. None of these look helpful.

The knowns and unknowns are currently:

```md
8 - abcdefg (7)

1 -   c  f  (2)
4 -  bcd f  (4)
7 - a c  f  (3)
5 - ab d fg (5)

0 - abc efg (6)
6 - ab defg (6)
9 - abcd fg (6)

2 - a cde g (5)
3 - a cd fg (5)
```

I had already computed combinations of `147`, but maybe there's new info now
that `5` is also known.

The unknowns, \\([0, 6, 9, 2, 3\\) have at least 5 active segments, so taking
operations which give at least 5 elements will be most useful.

The union of `15`s active segments is `abcdfg`, which matches the active
segments of `9`. Sweet!

The union of `45`'s active segments is `abcdfg`, which we've already determined
to be `9`. The union of `75`'s active segments doesn't yield anything new
either.

The knowns and unknowns are currently:

```md
8 - abcdefg (7)

1 -   c  f  (2)
4 -  bcd f  (4)
7 - a c  f  (3)
5 - ab d fg (5)
9 - abcd fg (6)

0 - abc efg (6)
6 - ab defg (6)

2 - a cde g (5)
3 - a cd fg (5)
```

The difference between `0` and `6` is that the former has a `c`, while the
latter has a `d`. Subtracting `5 abdfg` from `9 abcdfg` gives `c`, and therefore
distinguishes `0` from `6`.

The difference between `2` and `3` is that the former has an `e`, while the
latter has an `f`. The complement of the union of `14759`'s active segments is
`e`, and that can be used to distinguish `2` from `3`.

{{% comment %}}

Tedious exercise, but rewarding in the end. I feel like Sherlock Holmes.

{{% /comment %}}

```hs
sumOfOutputValues :: [SevenSegmentDisplay] -> Int
sumOfOutputValues _ = 0
```

## References

1. {{< citation
    id="containersHaskell"
    title="containers: Assorted concrete container types"
    url="https://hackage.haskell.org/package/containers"
    url_2="https://haskell-containers.readthedocs.io/en/latest/"
    url_3="https://github.com/haskell-perf/sets"
    accessed="2022-03-13" >}}

1. {{< citation
    id="Okasaki1998"
    title="Fast Mergeable Integer Maps"
    authors="Okasaki, Chris; Andy Gill"
    affiliations="Columbia University; Semantic Designs"
    publication="Workshop on ML, pp. 77-86"
    year="1998"
    url="http://ittc.ku.edu/~andygill/papers/IntMap98.pdf"
    url_2="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=Fast+Mergeable+Integer+Maps+(1998)&btnG="
    cited_by_count="91"
    cited_by_count_last_mod="2022-03-13"
    accessed="2022-03-13" >}}

1. {{< citation
    id="Morrison1968"
    author="Morrison, Donald R"
    title="PATRICIA - practical algorithm to retrieve information coded in alphanumeric."
    publication="Journal of the ACM, Vol. 15, No. 4 (1968): 514-534."
    url="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=PATRICIA%E2%80%94practical+algorithm+to+retrieve+information+coded+in+alphanumeric&btnG="
    cited_by_count="1370"
    cited_by_count_last_mod="2022-03-13"
    accessed="2022-03-13" >}}

1. {{< citation
    id="LeventErkok2020"
    title="Question: Confusion between `-Wall` and `NamedFieldPuns` (#18246) · Issues · Glasgow Haskell Compiler / GHC · GitLab"
    date="2020-05-27"
    url="https://gitlab.haskell.org/ghc/ghc/-/issues/18246"
    url_2="https://github.com/ndmitchell/hlint/issues/1250"
    accessed="2022-03-13" >}}

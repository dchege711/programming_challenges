---
date: 2022-03-16
domains:
- adventofcode.com
- en.wikipedia.org
- hackage.haskell.org
- wiki.haskell.org
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/AoC2021/SmokeBasin/
title: 'AoC 2021 Day 09: Smoke Basin'
weight: 9
---

{{< citation
    id="AoC2021-09"
    title="Day 9 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/9"
    accessed="2022-03-16" >}}

## Part I {{% cite AoC2021-09 %}}

These caves seem to be lava tubes. Parts are even still volcanically active;
small hydrothermal vents release smoke into the caves that slowly settles like
like rain.

If you can model how the smoke flows through the caves, you might be able to
avoid it and be that much safer. The submarine generates a height-map of the
floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. Your first goal is to find
the low points - the locations that are lower than any of its adjacent
locations. Most locations have four adjacent locations (up, down, left, and
right); locations on the edge or corner of the map have three or two adjacent
locations, respectively. (Diagonal locations do not count as adjacent.)

The risk level of a low point is `1` plus its height. Find all of the low points
on your height-map. **What is the sum of the risk levels of all low points on
your height-map?**

{{% comment %}}

The modeling of the risk level as `1 + height` is unintuitive. I'd think that
the lowest point on the map has the greatest risk, e.g. if there is a lot of
smoke flow, the global low point will get the greatest depth of smoke. So I'd
have modeled the risk level as `1 / (1 + height)`.

{{% /comment %}}

## Input Representation

The data is an \\(n \times n\\) grid. In another language, using a
one-dimensional array might have reaped me the benefits of cache locality, but
lists in Haskell are linked lists that lack guaranteed locality.

In [Day 04: Giant Squid]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/GiantSquid#HiddingAoC2021-04"
\>}}), Hidding used the {{% cite Massiv %}} array library, whose tagline is
"multi-dimensional arrays with fusion, stencils and parallel computation". It's
at least worth checking out in the problem.

{{% comment %}}

A stencil is a function that can read the neighboring elements of the stencil's
center (the zero index), and only those, and then outputs a new value for the
center element. {{% cite Massiv %}}

{{% cite Massiv %}} mentions "Moore neighborhood", which led to me to the Von
Neumann neighborhood (the cell itself and cells at a Manhattan distance of `1`)
which happens to be the kind of neighborhood being evaluated in this problem.
{{% cite wikiVonNeumannNeighborhood %}}

{{% /comment %}}

{{% comment %}}

For example, `foo = length . filter p . map g . map f . concat` would be pretty
inefficient if executed literally. Fusion refers to program transformations
aimed at removing intermediate data structures. GHC has transformation rules
that enable fusion. {{% cite haskellWikiFusion %}}

Also encountered stream fusion [in the `Data.Vector` library]({{< ref
"../BinaryDiagnostic/03-binary-diagnostic.md#Data.Vector.StreamFusion" >}}). A
*stream* represents the traversal of a list-like structure. All the list
functions become stream functions --  but crucially. stream operations are
non-recursive, and can therefore be glued together. {{% cite haskellWikiFusion
%}}

{{% /comment %}}

## References

1. {{< citation
    id="Massiv"
    title="massiv: Massiv (Массив) is an Array Library."
    url="https://hackage.haskell.org/package/massiv"
    url_2="https://hackage.haskell.org/package/massiv#stencil"
    accessed="2022-03-16" >}}

1. {{< citation
    id="wikiVonNeumannNeighborhood"
    title="Von Neumann neighborhood - Wikipedia"
    url="https://en.wikipedia.org/wiki/Von_Neumann_neighborhood"
    accessed="2022-03-16" >}}

1. {{< citation
    id="haskellWikiFusion"
    title="GHC optimisations - HaskellWiki"
    url="https://wiki.haskell.org/GHC_optimisations#Fusion"
    accessed="2022-03-16" >}}

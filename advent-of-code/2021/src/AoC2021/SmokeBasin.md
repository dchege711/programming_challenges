---
authors:
- Hidding, Johan
date: 2022-03-16
domains:
- adventofcode.com
- en.cppreference.com
- en.wikipedia.org
- hackage.haskell.org
- jhidding.github.io
- stackoverflow.com
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

```hs
module AoC2021.SmokeBasin (
    HeightMap, sumOfRiskLevelsOfLowPoints, productOf3LargestBasins)
where

import Data.Massiv.Core.Index (Ix2(..), Sz(..), Border(..))
import qualified Data.Massiv.Array as Massiv.Array (Array, P(..), computeAs, sum)
import Data.Massiv.Array.Stencil (Stencil, makeStencil, mapStencil)
import Data.List (foldl')
```

## Input Representation

The data is an \\(n \times n\\) grid. In another language, using a
one-dimensional array might have reaped me the benefits of cache locality, but
lists in Haskell are linked lists that lack guaranteed locality.

In [Day 04: Giant Squid]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/GiantSquid#HiddingAoC2021-04"
\>}}), Hidding used the {{% cite Massiv %}} array library, whose tagline is
"multi-dimensional arrays with [fusion](#MassiveFusion),
[stencils](#MassiveStencil) and parallel computation". It's at least worth
checking out in the problem.

## Notable Design Decisions in `massiv`

For example, `foo = length . filter p . map g . map f . concat` would be pretty
inefficient if executed literally. Fusion refers to program transformations
aimed at removing intermediate data structures. GHC has transformation rules
that enable fusion. {{% cite haskellWikiFusion %}}  <a id="MassiveFusion"></a>

{{% comment %}}

Also encountered stream fusion [in the `Data.Vector` library]({{< ref
"../BinaryDiagnostic/03-binary-diagnostic.md#Data.Vector.StreamFusion" >}}). A
*stream* represents the traversal of a list-like structure. All the list
functions become stream functions --  but crucially. stream operations are
non-recursive, and can therefore be glued together. {{% cite haskellWikiFusion
%}}

{{% /comment %}}

[Like `Data.Vector`]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/03-binary-diagnostic#efficiency-of-collections-types"
\>}}), `Massiv` also supports boxed elements (may point to a thunk), unboxed
elements, and storable types. However, `Massiv` has optimized containers for
instances of [the `Prim`
class](https://hackage.haskell.org/package/massiv-1.0.1.1/docs/Data-Massiv-Array-Manifest.html#t:Prim).
{{% cite Massiv %}}

{{% comment %}}

A bit surprised that `Boolean` is not an instance of the `Prim` type. Other
languages tend to have Boolean as one of primitive types.

The size of a `bool` in C++ is defined from the implementation and may be
greater than 1 (byte) {{% cite cppFundamentalTypes %}}. However, the `sizeof`
for `char`, `signed char`, `unsigned char`, `std::byte` (C++17), and `char8_t`
(C++20) will always evaluate to 1 (byte). {{% cite cppSizeOf %}}. A tad
counter-intuitive that a `bool` is at least 8 bits in C++. {{% cite
cpp8bitsForBoolean %}} attributes this to every C++ data type needing to be
addressable, and most CPU architectures are designed with a 8-bit chunks as the
smallest addressable memory.

While `std::vector<bool>` is allowed to be space-efficient,
it loses some guarantees of `std::vector` e.g. safely modifying elements
concurrently in a multi-threaded context, contiguous storage (that allows
pointer arithmetic), etc. {{% cite cppVectorBool %}}

{{% /comment %}}

`Massiv` has delayed arrays which do not exist in memory, and are instead
defined as a function or a composition of functions. This allows to operate on
a massive array in constant memory. {{% cite Massiv %}}

`Massiv` has a wrapping data type for indices, e.g. `makeArrayR D Seq
(Sz (3 :. 5)) (\ (i :. j) -> i * j)`, which creates a 2D array with 3 arrays,
each with 5 elements, where the element at index `i :. j` is computed as
`i * j`. There is a constructor, `IxN` that supports N-dimensional arrays, e.g.
`10 :> 20 :> 30 :. 40` is an index into a 4D array. {{% cite Massiv %}}

A stencil is a function that can read the neighboring elements of the stencil's
center (the zero index), and only those, and then outputs a new value for the
center element. {{% cite Massiv %}} <a id="MassiveStencil"></a>

{{% comment %}}

{{% cite Massiv %}} mentions "Moore neighborhood", which led to me to the Von
Neumann neighborhood (the cell itself and cells at a Manhattan distance of `1`)
which happens to be the kind of neighborhood being evaluated in this problem.
{{% cite wikiVonNeumannNeighborhood %}}

{{% /comment %}}

## Input Representation (Cont'd)

```hs
--  `A.P` because the underlying representation (Int) is an instance of the
--  `Prim` type class.
type HeightMap = Massiv.Array.Array Massiv.Array.P Ix2 Int
```

{{% comment %}}

For a while, I was stuck thinking of \\(a = [[0,1,2], [1,2,5], ..., [3,2,6]]\\)
as an N-dimensional array, where \\(N\\) can only be determined after reading
the whole file. Gleaning at {{% cite HiddingAoC21-09 %}} made me see \\(a\\) as
the 2D array that it is.

{{% /comment %}}

## Part I Solution

```hs
vonNeumannNeighborhood :: (Ix2 -> Int) -> [Int]
vonNeumannNeighborhood get =
    [get (-1 :. 0), get (1 :. 0), get (0 :. -1), get (0 :. 1)]

riskIfLowPoint :: (Ix2 -> Int) -> Int
riskIfLowPoint get =
    let centerPoint = get (0 :. 0)
        lowPoint = foldl' min (maxBound :: Int) (vonNeumannNeighborhood get)
        risk
            | centerPoint < lowPoint = 1 + centerPoint
            | otherwise              = 0
    in  risk

riskIfLowPointStencil :: Stencil Ix2 Int Int
riskIfLowPointStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) riskIfLowPoint
{-#  INLINE riskIfLowPointStencil  #-}

sumOfRiskLevelsOfLowPoints :: HeightMap -> Int
sumOfRiskLevelsOfLowPoints heightMap =
    let borderHandling = Fill (maxBound :: Int)
        risksArrayDW = mapStencil borderHandling riskIfLowPointStencil heightMap
        riskArray = Massiv.Array.computeAs Massiv.Array.P risksArrayDW
    in Massiv.Array.sum riskArray
```

{{% comment %}}

Most of my time was spent trying to get the syntax and types right.

{{% /comment %}}

## Part II {{% cite AoC2021-09 %}}

Next, you need to find the largest basins so you know what areas are most
important to avoid.

A basin is all locations that eventually flow downward to a single low point.
Therefore, every low point has a basin, although some basins are very small.
Locations of height `9` do not count as being in any basin, and all other
locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the
low point.

What do you get if you multiply together the sizes of the three largest basins?

```hs
productOf3LargestBasins :: HeightMap -> Int
productOf3LargestBasins _ = 0
```

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

1. {{< citation
    id="cppFundamentalTypes"
    title="Fundamental types - cppreference.com"
    url="https://en.cppreference.com/w/cpp/language/types#Boolean_type"
    accessed="2022-03-28" >}}

1. {{< citation
    id="cppSizeOf"
    title="sizeof operator - cppreference.com"
    url="https://en.cppreference.com/w/cpp/language/sizeof"
    accessed="2022-03-28" >}}

1. {{< citation
    id="cpp8bitsForBoolean"
    title="boolean - C++ : why bool is 8 bits long? - Stack Overflow"
    url="https://stackoverflow.com/questions/2064550/c-why-bool-is-8-bits-long"
    accessed="2022-03-28" >}}

1. {{< citation
    id="cppVectorBool"
    title="std::vector<bool> - cppreference.com"
    url="https://en.cppreference.com/w/cpp/container/vector_bool"
    accessed="2022-03-28" >}}

1. {{< citation
    id="HiddingAoC21-09"
    author="Johan Hidding"
    title="Advent of Code 2021: Day 9: Smoke Basin"
    url="https://jhidding.github.io/aoc2021/#day-9-smoke-basin"
    accessed="2022-03-29" >}}

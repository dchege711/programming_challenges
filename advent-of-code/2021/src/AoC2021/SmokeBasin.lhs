%include polycode.fmt
---
title: "AoC 2021 Day 09: Smoke Basin"
date: 2022-03-16
weight: 9
---

{{< citation
    id="AoC2021-09"
    title="Day 9 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/9"
    accessed="2022-03-16" >}}

\## Part I {{% cite AoC2021-09 %}}

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

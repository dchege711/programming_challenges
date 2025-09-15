---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/08-resonant-collinearity/08-resonant-collinearity/
title: 'AoC 2024 Day 08: Resonant Collinearity'
---

## Parsing

Each antenna in the map is tuned to a frequency indicated by a single lowercase
letter, uppercase letter, or digit. {{% cite AoC2024Day08 %}}

Two antennas with the same frequency create two collinear antinodes on either
side where one of the antennas is twice as far away as the other, e.g.

<table>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>f</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>f</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
</table>

Antinodes can occur at locations that contain other antennas. {{% cite
AoC2024Day08 %}}

[Part One](#part-one) requires computing locations of antinodes. Given antennas
with frequency \\(f\\), I need to pairwise match them and compute their
antinodes. Being able to group all such antennas is useful for this puzzle.

<details>
<summary>ResonantCollinearity.Parse.cs</summary>

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/08-resonant-collinearity/ResonantCollinearity.Parse.cs"
  highlight="cs"
  id="ResonantCollinearity.Parse.cs" >}}

</details>

## Part One

How many unique locations within the bounds of the map contain an antinode? {{%
cite AoC2024Day08 %}}

<details>
<summary>Sample input with answer overlay</summary>

<table>
<tr><td></td><td>0</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td><td>10</td><td>11</td></tr>
<tr><td>0</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td></tr>
<tr><td>1</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>0</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>2</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>0</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td></tr>
<tr><td>3</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>0</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>4</td><td>.</td><td>.</td><td>.</td><td>.</td><td>0</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td></tr>
<tr><td>5</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>A</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>6</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>7</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>8</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>A</td><td>.</td><td>.</td><td>.</td></tr>
<tr><td>9</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>A</td><td>.</td><td>.</td></tr>
<tr><td>10</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td></tr>
<tr><td>11</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>.</td><td>#</td><td>.</td></tr>
</table>

</details>

Given \\(N\\) antennas with the same frequency, each pairing generates 2
antinodes. There are \\(\frac{N(N-1)}{2}\\) possible pairings and so we have
\\(\mathcal{O}(N^2)\\) work to do for each frequency.

The same \\((r_i, c_i), (r_j, c_j)\\) computes the same antinode positions
regardless of the associated frequency. Caching \\((r_i, c_i, r_j, c_j)\\) can
save us from repeating some of the \\(\mathcal{O}(N^2)\\) work because all we
care about are unique antinode positions.

Because the antenna locations are ordered based on how we encountered them on
the map, we don't need to cache both \\((r_i, c_i, r_j, c_j)\\) and \\((r_j,
c_j, r_i, c_i)\\).

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/08-resonant-collinearity/ResonantCollinearity.PartOne.cs"
  highlight="cs"
  id="ResonantCollinearity.PartOne.cs" >}}

## Part Two

An antinode occurs at any grid position that is collinear with at least two
antennas of the same frequency. How many unique locations within the bounds of
the map contain an antinode? {{% cite AoC2024Day08 %}}

The challenge here is in the explosion of antinode locations. For example, in an
\\(N \times N\\) grid, matching antennas at \\((a, a)\\) and \\((b, b)\\) lead
to \\(N\\) antinode positions along the diagonal.

If the optimizations in [Part One](#part-one) are insufficient, then we might
want to cache a combination of a unit vector and the top/left cell to avoid
repeated computation. Turns out the optimizations are sufficient; won't
complicate it further.

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/08-resonant-collinearity/ResonantCollinearity.PartTwo.cs"
  highlight="cs"
  id="ResonantCollinearity.PartTwo.cs" >}}

## References

1. {{< citation
  id="AoC2024Day08"
  title="Day 08 - Advent of Code 2024: Resonant Collinearity"
  url="https://adventofcode.com/2024/day/8"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

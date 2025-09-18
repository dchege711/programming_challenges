---
cited-authors:
- Wastl, Eric
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/09-disk-fragmenter/09-disk-fragmenter/
title: 'AoC 2024 Day 09: Disk Fragmenter'
---

## Parsing

A disk map like `12345` represents a 1-block file, 2 blocks of free space, a
3-block file, 4 blocks of free space, and then a 5-block file. {{% cite
AoC2024Day09 %}}

Each file has an ID number that is zero-indexed based on the position of the
file, e.g., the disk map `12345` represents these blocks `0..111....22222`. {{%
cite AoC2024Day09 %}}

While [Part One](#part-one) will benefit from a concrete collection because it
needs to access the list from both ends, the parser should return an
`IEnumerable` and have the client decide which concrete collection to form.

<details>
<summary>DiskFragmenter.Parse.cs</summary>

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/09-disk-fragmenter/DiskFragmenter.Parse.cs"
  highlight="cs"
  id="DiskFragmenter.Parse.cs" >}}

</details>

## Part One

Move file blocks one at a time from the end of the disk to the leftmost free
space block until there are no gaps remaining between file blocks, e.g.,

```txt
0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......
```

{{% cite AoC2024Day09 %}}

Compute the filesystem's checksum by multiplying each of these blocks' position
with the contained file ID number and summing these values. The leftmost block
is in position `0`. For example, the checksum of `022111222......` is \\(0 \cdot
0 + 1 \cdot 2 + 2 \cdot 2 + 3 \cdot 1 + 4 \cdot 1 + 5 \cdot 1 + 6 \cdot 2 + 7
\cdot 2 + 8 \cdot 2\\). {{% cite AoC2024Day09 %}}

The challenge to this problem is the fact that the list expands a great deal,
e.g., `999` represents `000000000.........111111111`. Can we solve this problem
without holding the expanded list in memory? Lots of bookkeeping involved in
such an approach.

## References

1. {{< citation
  id="AoC2024Day09"
  title="Day 09 - Advent of Code 2024: Disk Fragmenter"
  url="https://adventofcode.com/2024/day/9"
  author="Eric Wastl"
  accessed="2025-08-23" >}}

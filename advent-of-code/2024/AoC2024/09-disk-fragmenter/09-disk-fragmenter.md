---
date: 2025-08-23
domains:
- adventofcode.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/09-disk-fragmenter/09-disk-fragmenter/
title: 'AoC 2024 Day 09: Disk Fragmenter'
---

## Problem Statement

### Part One

You notice an amphipod trying to make more contiguous free space on his computer
by compacting all of the files, but his program isn't working; you offer to
help.

He shows you the **disk map** he's already generated:

{{< readfile
  file="content/computer-science/programming-challenges/advent-of-code/2024/AoC2024.Tests/data/day-09-sample.in.txt"
  highlight="txt" >}}

The disk map uses a dense format to represent the layout of **files** and **free
space** on the disk. The digits alternate between indicating the length of a
file and the length of free space.

So, a disk map like `12345` would represent a one-block file, two blocks of free
space, a three-block file, four blocks of free space, and then a five-block
file.

Each file on disk also has an **ID number** based on the order of the files as
they appear **before** they are rearranged, starting with ID `0`. So, the disk
map `12345` has three files: a one-block file with ID `0`, a three-block file
with ID `1`, and a five-block file with ID `2`. Using one character for each
block where digits are the file ID and `.` is free space, the disk map `12345`
represents these individual blocks: `0..111....22222`

The amphipod would like to **move file blocks one at a time** from the end of
the disk to the leftmost free space block (until there are no gaps remaining
between file blocks). For the disk map `12345`, the process looks like this:

```txt
0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......
```

The final step of this file-compacting process is to update the **filesystem
checksum**. To calculate the checksum, add up the result of multiplying each of
these blocks' position with the file ID number it contains. The leftmost block
is in position 0. If a block contains free space, skip it instead.

Compact the amphipod's hard drive using the process he requested. **What is the
resulting filesystem checksum?**

1. {{< citation
  id="AoC2024Day09"
  title="Day 09 - Advent of Code 2024: Disk Fragmenter"
  url="https://adventofcode.com/2024/day/9"
  accessed="2025-08-23" >}}

---
date: 2022-07-26
domains:
- isocpp.github.io
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/performance/general-tips/
title: General Tips on Performance and Optimization
weight: 1
---

Trying to optimize may lead to errors and high-maintenance/change-resistant
code. If there's no need to optimize, let the code be. {{% cite
isoCPPPerformance %}}

Only optimize performance-critical parts of a program. If your program spends 4%
of its processing time doing computation A, and 40% of its time doing
computation B, a 50% improvement on A is only as impactful as a 5% improvement
on B. {{% cite isoCPPPerformance %}}

Complicated and/or low-level code is not necessarily faster than simple code.
Optimizers sometimes do marvels with simple and/or high-level code. {{% cite
isoCPPPerformance %}}

{{% open-comment %}}

Where can I find latest compiler tips and tricks, so that I can gain an
intuition of what the compiler likes?

{{% /open-comment %}}

Don't make claims about performance without measuring your code. Tools include:
microbenchmarks, e.g., using Unix `time`; profilers, to identify performance
critical parts of your system, etc. {{% cite isoCPPPerformance %}}

Pass sufficient information in the interface for a good implementation to be
chosen. For example, `void qsort(void* base, size_t num, size_t size, int
(*compar)(const void*, const void*));` throws away useful information (e.g., the
element type), forces the user to repeat already-known information (e.g., the
element size), and forces the user to write extra code (e.g., a function to
compare two elements). Given that the compiler knows about the size of the
array, the type of elements, and how to elements, an interface like
`template<typename Iter> void sort(Iter b, Iter e)` is better. {{% cite
isoCPPPerformance %}}

1. {{< citation
  id="isoCPPPerformance"
  title="C++ Core Guidelines > Per: Performance"
  url="https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#S-performance"
  accessed="2022-07-26" >}}

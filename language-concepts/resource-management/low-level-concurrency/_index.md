---
date: 2026-10-21
domains:
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/low-level-concurrency/
title: Low Level Concurrency
---

When a program runs, the OS creates a process -- an isolated container with its
own address space and resources. Inside that process, the OS or language runtime
can create **threads**. A thread is an independent execution path with its own
program counter, registers, and stacks, but it shares the heap, globals, and
open resources with other threads in the same process. Concurrency exists
whenever multiple threads can make progress independently and their execution
can overlap. Even in a single-core environment, the OS rapidly switches between
threads and interleaves their instructions. {{% cite HelloInterviewConcurrency
%}}

{{< citation
  id="HelloInterviewConcurrency"
  title="Introduction to Concurrency | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/concurrency/intro"
  accessed="2026-10-21" >}}

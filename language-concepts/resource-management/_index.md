---
authors:
- Stroustrup, Bjarne
date: 2022-05-31
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/
title: Resource Management
---

A **resource** is something that must be acquired, and later (explicitly
or implicitly) released. Examples include memory, locks, sockets, thread
handles, and file handles. {{% cite Stroustrup2018-Ch13 %}}

In a long-running program, failing to release a resource may degrade
performance or even crash. {{% cite Stroustrup2018-Ch13 %}} Worse still,
one may introduce security bugs, e.g. use-after-free.

{{< citation
  id="Stroustrup2018-Ch13"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 13. Utilities"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018">}}

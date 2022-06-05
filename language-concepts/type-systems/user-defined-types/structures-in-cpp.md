---
authors:
- Stroustrup, Bjarne
date: 2022-06-04
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/structures-in-cpp/
title: Structures in C++
weight: 20
---

A **struct** helps us organize the elements that a type needs into a
data structure, e.g.

```cpp
struct Vector {
  int sz;         // number of elements
  double* elem;   // pointer to elements on the free store
};

void vector_init(Vector& v, int s) {
  v.elem = new double[s];
  v.sz = s;
}
```

{{% cite Stroustrup2018-Ch2 %}}

However, notice that a user of `Vector` has to know every detail of a
`Vector`'s representation. We can improve on this. {{% cite
Stroustrup2018-Ch2 %}}

There is no fundamental difference between a `struct` and a `class`; a
`struct` is simply a `class` with members public by default. {{% cite
Stroustrup2018-Ch2 %}}

1. {{< citation
  id="Stroustrup2018-Ch2"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 2. User-Defined Types"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018" >}}

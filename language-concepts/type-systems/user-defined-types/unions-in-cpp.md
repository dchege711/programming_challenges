---
cited-authors:
- Stroustrup, Bjarne
date: 2022-06-04
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/unions-in-cpp/
title: Unions in C++
weight: 40
---

A **union** is a `struct` in which all members are allocated at the same
address so that the `union` occupies as much space as its largest
member, e.g.

```cpp
// This is a naked union because it doesn't have an associated indicator
// for which member it holds.
union Value {
  Node* p;
  int i;
}

// The language doesn't keep track of which kind of value is held by a
// union, so the programmer must do that themselves, e.g.

enum Type { ptr, num };

struct Entry {
  string name;
  Type t;
  Value v;      // use v.p if t == ptr; use v.i if t == num
};

// Sample usage.
void f(Entry* pe) {
  if (pe->t == num)
    std::cout << pe->v.i;
  // ...
}
```

{{% cite Stroustrup2018-Ch2 %}}

Maintaining the correspondence between `Entry`'s `t` and the type held
in the union is error-prone. One approach is to encapsulate the union
and type-field in a class, and only offer access through member
functions that use the union correctly. {{% cite Stroustrup2018-Ch2 %}}

The STL type, `variant`, can be used to eliminate most direct uses of
unions, e.g.

```cpp
struct Entry {
  string name;
  std::variant<Node*, int> v;
}

void f(Entry* pe) {
  if (std::holds_alternative<int>(pe->v))
    std::cout << std::get<int>(pe->v);
  // ...
}
```

{{% cite Stroustrup2018-Ch2 %}}

{{% open-comment %}}

{{% cite Stroustrup2018-Ch2 %}} notes, "For many uses, a `variant` is
simpler and safer to use than a `union`." When would we ever prefer a
`union` to a `variant`?

{{% /open-comment %}}

1. {{< citation
  id="Stroustrup2018-Ch2"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 2. User-Defined Types"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018" >}}

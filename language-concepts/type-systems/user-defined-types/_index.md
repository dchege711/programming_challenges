---
cited-authors:
- Stroustrup, Bjarne
date: 2022-06-04
domains:
- en.cppreference.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/
title: User-Defined Types
weight: 1
---

## Definition from C++

**Built-in types** are ones that can be built from the fundamental types
(e.g. `void`, `std::nullptr_t`, `int`, `bool`, `char`, `float`, `double`
{{% cite cppReferenceFundamentalTypes %}}), the `const` modifier, and
the declarator operators (e.g. `int[3]`, `int*`, `int&`). {{% cite
Stroustrup2018-Ch2 %}}

While the built-in types directly and efficiently represent the
capabilities of conventional computer hardware, they're too low-level to
conveniently write advanced applications in. C++ abstraction mechanisms
let programmers design and implement **user-defined types** using both
built-in types and other user-defined types. {{% cite Stroustrup2018-Ch2
%}}

{{% comment %}}

Before this, I thought user-defined types did _not_ include STL types. I
thought that user meant me, or library authors, but not the C++ language
devs, who, I assume, write the STL.

{{% /comment %}}

User-defined types are often easier to use, less error prone, and
typically as efficient as direct use of built-in types. {{% cite
Stroustrup2018-Ch2 %}}

{{% open-comment %}}

{{% cite Stroustrup2018-Ch2 %}} says that sometimes user-defined types
can be more efficient than built-in types. When would this be? Maybe
something like
[`std::vector<bool>`](https://en.cppreference.com/w/cpp/container/vector_bool),
which is possibly (implementation-defined) more space-efficient than a
`std::vector` (and thus an array) of `bool`?

{{% /open-comment %}}

1. {{< citation
  id="Stroustrup2018-Ch2"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 2. User-Defined Types"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018" >}}

1. {{< citation
  id="cppReferenceFundamentalTypes"
  title="Fundamental types - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/types"
  accessed="2022-06-04" >}}

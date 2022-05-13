---
authors:
- Stroustrup, Bjarne
date: 2022-05-12
domains:
- en.cppreference.com
- en.wikipedia.org
- isocpp.org
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/classes-in-cpp/
title: Classes in C++
---

{{% open-comment %}}

{{% cite Stroustrup2018 %}} quotes Doug McIlroy:

> Those types are not "abstract"; they are as real as `int` and `float`.

What is the context of this quote?

{{% /open-comment %}}

A class is a user-defined type provided to represent a concept in the
code of a program. Essentially, all language facilities beyond the
fundamental types, operators, and statements exist to help define better
class or to use them more conveniently. {{% cite Stroustrup2018 %}}

{{% comment %}}

Sometimes I have problems defining the concept that is going to be
encapsulated as a class. Other times, the naming is hard, and I result
to common patterns like `FooManager`.

{{% /comment %}}

## Concrete Types

The basic idea of concrete classes is that they behave "just like
built-in types". The defining characteristic of a concrete type is that
its representation is part of its definition. If the representation
changes in any significant way, a user must recompile. {{% cite
Stroustrup2018 %}}

For some concrete types, e.g. `std::vector` and `std::string`, the
representation may have pointers to data stored in the free store
(dynamic memory, heap). Such types can be considered resource handles
with carefully crafted interfaces. {{% cite Stroustrup2018 %}}

The "representation is part of definition" property allows us to:

* Place objects of concrete types on the stack and in other objects.
* Refer to objects directly (and not just through pointers or references).
* Initialize objects immediately and completely.
* Copy and move objects.

{{% cite Stroustrup2018 %}}

```cpp
// From Stroustrup2018

class complex {
  double re, im; // representation: two doubles

 public:
  complex(double r, double i) :re(r), im(i) {}
  complex(double r) :re(r), im{0} {}
  complex() :re{0}, im{0} {}

  double real() const { return re; }
  void real(double d) { re = d; }
  double imag() const { return im; }
  void imag(double d) { im = d; }

  complex& operator+=(complex z) {
    re += z.re;
    im += z.im;
    return *this;
  }

  complex& operator-=(complex z) {
    re -= z.re;
    im -= z.im;
    return *this;
  }

  complex& operator*=(complex); // defined out-of-class somewhere
  complex& operator/=(complex); // defined out-of-class somewhere
};
```

{{% comment %}}

{{% cite Stroustrup2018 %}} notes that `complex`'s representation has
to be compatible with what Fortran provided 60 years ago.

Why is C++'s compatibility with Fortran important? [Intel says something
similar](https://www.intel.com/content/www/us/en/develop/documentation/onemkl-macos-developer-guide/top/language-specific-usage-options/mixed-language-programming-with-onemkl/using-complex-types-in-c-c.html).
It seems that mixing Fortran and C++ is commonplace, e.g. [IBM's
instructions](https://www.ibm.com/docs/en/xl-fortran-aix/15.1.2?topic=calls-mixing-fortran).

{{% /comment %}}

### Inlining

Simple operations (such as constructors, `+=`, `imag`, etc.) must be
inlined (implemented without function calls in the generated machine
code) {{% cite Stroustrup2018 %}}. {{% cite isoCPPInlineFunctions %}}
paints a more subtle picture of the effect of inlining on performance.
Inline functions might make the executable:

* Larger as the inlined functions get expanded in multiple
  places.
* Smaller if the expansion generates less code than the code to
  push/pop registers/parameters.
* Faster by removing unnecessary instructions, increasing cache
  hits because of improved locality.
* Slower if it's larger and therefore more thrashing.

{{% comment %}}

Thrashing occurs when a computer's virtual memory resources are
overused, leading to a constant state of paging and page faults. {{%
cite wikiThrashing %}}

{{% /comment %}}

Inlining might even be irrelevant to speed because most systems are not
CPU-bound (most are I/O-bound, database-bound, or network-bound). {{%
cite isoCPPInlineFunctions %}}

Compilers are not obligated to `inline` (or not to `inline`). The
meaning of `inline` has evolved to be "multiple definitions are
permitted" rather than "inlining is preferred". {{% cite
cppReferenceInlineSpecifier %}}

### Default Constructors

By defining a default constructor (one that can be invoked without
arguments), one eliminates the possibility of uninitialized variables of
that type. {{% cite Stroustrup2018 %}} One can also have `complex() =
delete;`, which will cause a compiler error if the default constructor
gets selected. {{% cite cppReferenceDefaultConstructor %}}

### Out-of-Class Operator Definitions

Operations that do not require direct access to the representation can
be defined separately from the class definition, e.g.

```cpp
complex operator+(complex a, complex b) { return a += b; }
complex operator-(complex a, complex b) { return a -= b; }
complex operator-(complex a) { return {-a.real(), -a.imag()}; }
complex operator*(complex a, complex b) { return a *= b; }
complex operator/(complex a, complex b) { return a /= b; }
```

An argument passed by value is copied, and therefore it can be modified
without affecting the caller's copy. {{% cite Stroustrup2018 %}} Had
we received the arguments by reference, implementing `a + b` as `a += b`
because of performance may lead to buggy programs because users expect
`a + b` to make a copy. {{% cite isoCPPOperatorOverloading %}}

{{% comment %}}

The `return a += b;` statement caught me off-guard. Looks like invalid
syntax.

{{% /comment %}}

ISOCPP has considered letting users define their own operators several
times, and the answer has always been that the likely problems outweigh
the likely benefits {{% cite isoCPPOperatorOverloading %}}.

{{% comment %}}

Other languages, like Haskell, allow custom operators. I've sometimes
found the syntax unintuitive, e.g. `<|>`.

{{% /comment %}}

Some operators, e.g. `.`, `::`, `sizeof`, and `?:`, cannot be
overridden. Furthermore, one can't define an operator all of whose
operands/parameters are of primitive types. {{% cite
isoCPPOperatorOverloading %}}

## References

1. {{< citation
  id="Stroustrup2018"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 4. Classes"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018">}}

1. {{< citation
  id="cppReferenceInlineSpecifier"
  title="inline specifier"
  url="https://en.cppreference.com/w/cpp/language/inline"
  accessed="2022-05-12" >}}

1. {{< citation
  id="cppReferenceDefaultConstructor"
  title="Default constructors"
  url="https://en.cppreference.com/w/cpp/language/default_constructor"
  accessed="2022-05-12" >}}

1. {{< citation
  id="isoCPPOperatorOverloading"
  title="Operator Overloading, C++ FAQ"
  url="https://isocpp.org/wiki/faq/operator-overloading"
  accessed="2022-05-13" >}}

1. {{< citation
  id="isoCPPInlineFunctions"
  title="Inline Functions, C++ FAQ"
  url="https://isocpp.org/wiki/faq/inline-functions#inline-member-fns-more"
  accessed="2022-05-13" >}}

1. {{< citation
  id="wikiThrashing"
  title="Thrashing (computer science)"
  url="https://en.wikipedia.org/wiki/Thrashing_(computer_science)"
  accessed="2022-05-13" >}}

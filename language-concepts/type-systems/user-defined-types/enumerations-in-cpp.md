---
authors:
- Stroustrup, Bjarne
date: 2022-06-04
domains:
- en.cppreference.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/enumerations-in-cpp/
title: Enumerations in C++
weight: 30
---

## Unscoped (or Plain or C-Style) Enumerations

Plain (or C-style) enums are entered in the same scope as the name of
their enum, and implicitly convert to their integer value, e.g.

```cpp
enum Color { red, green, blue };
int col = green;

enum CardColor { red, black }; // Error: "red" conflicts with a previous declaration
```

{{% cite Stroustrup2018-Ch2 %}}

The name can be omitted if it's not going to be used, e.g.

```cpp
enum { red, green, blue };
int col = red;
```

{{% cite cppReferenceEnumerationDeclaration %}}

The enumerator values can be specified. If omitted, the default values
are zero for the first enum, and then

```cpp
enum Foo { a, b, c = 0, d, e = 10, f = b + 30};
// a = 0, b = 1, c = 0, d = 1, e = 10, f = 31
```

{{% cite cppReferenceEnumerationDeclaration %}}

## Scoped Enumerations

```cpp
enum class Color { red, blue, green };

enum class TrafficLight { red, green, yellow };

// The keywords `class` and `struct` are exactly equivalent.
// {{% cite cppReferenceEnumerationDeclaration %}}
enum struct CardColor { red, black };
```

{{% cite Stroustrup2018-Ch2 %}}

A scoped enumeration is strongly typed (`Color c = 2;` is a compiler
error), and its enumerators are scoped (`Color c = Color::red;`
compiles, but not `Color c = red;`) {{% cite Stroustrup2018-Ch2 %}}

The scoping prevents collisions between enumerator names, e.g.
`Color::red` is different from and can co-exist with
`TrafficLight::red`. {{% cite Stroustrup2018-Ch2 %}}

## Underlying Type

By default `enum Color { one, two }` is backed by an
implementation-defined integral type that can represent all enumerator
values. If no integral type can represent all the enumerator values, the
enumeration is ill-formed. {{% cite cppReferenceEnumerationDeclaration
%}}

Unscoped enums can implicit convert to their underlying type, but need a
static cast when being initialized from the underlying type, e.g.

```cpp
enum Color { red, green, blue };
int c1 = green; // Often undesirable as bugs may creep in.
Color c2 = 1; // error: invalid conversion from 'int' to 'Color'
Color c3 = static_cast<Color>(999); // OK, even if 999 is not a named enumerator
```

{{% cite cppReferenceEnumerationDeclaration %}}

From C++17, an enum may be initialized from an integer, without a cast,
via list initialization, e.g.

```cpp
enum Foo : unsigned char {}; // Foo can hold enumerators w/ values in [0, 255]

Foo f1{255}; // OK

Foo f2{-1}; // error: narrowing conversion of '-1' from 'int' to 'unsigned char' [-Wnarrowing]
Foo f3{256}; // error: narrowing conversion of '256' from 'int' to 'unsigned char' [-Wnarrowing]

int bar() { return 255; }
Foo f4{bar()}; // warning: narrowing conversion of 'bar()' from 'int' to 'unsigned char' [-Wnarrowing]
```

{{% cite cppReferenceEnumerationDeclaration %}}

To get the underlying value, one can use the `underlying_type` utils:

```cpp
enum Foo : char {};

Foo f{'b'};

std::underlying_type_t<Foo> v1 = static_cast<std::underlying_type_t<Foo>>(f);
std::underlying_type_t<Foo> v2 = std::to_underlying(f); // Shorthand; C++23

static_assert(std::is_same_v<char, decltype(std::to_underlying(f))>);
```

{{% cite cppReferenceToUnderlying %}}

## Operators for Enumerations

By default, an `enum class` only has assignment, initialization, and
comparisons. However, it is a user-defined type, and so we can define
operators for it, e.g.

```cpp
TrafficLight& operator++(TrafficLight& t) {
  switch (t) {
    case TrafficLight::green:   return t = TrafficLight::yellow;
    case TrafficLight::yellow:  return t = TrafficLight::red;
    case TrafficLight::red:     return t = TrafficLight::green;
  }
}

// Sample usage
TrafficLight light = TrafficLight::red;
TrafficLight next = ++light; // next becomes `TrafficLight::green`.
```

{{% cite Stroustrup2018-Ch2 %}}

## References

1. {{< citation
  id="Stroustrup2018-Ch2"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 2. User-Defined Types"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018" >}}

1. {{< citation
  id="cppReferenceEnumerationDeclaration"
  title="Enumeration declaration - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/enum"
  accessed="2022-06-05" >}}

1. {{< citation
  id="cppReferenceToUnderlying"
  title="std::to_underlying - cppreference.com"
  url="https://en.cppreference.com/w/cpp/utility/to_underlying"
  url_2="https://en.cppreference.com/w/cpp/types/underlying_type"
  accessed="2022-06-05" >}}

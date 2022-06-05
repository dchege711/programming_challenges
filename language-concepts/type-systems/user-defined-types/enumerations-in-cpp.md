---
authors:
- Stroustrup, Bjarne
date: 2022-06-04
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/enumerations-in-cpp/
title: Enumerations in C++
---

C++ offers a simple form of user-defined type for which we can enumerate
the values:

```cpp
// The "class" signifies that the enumeration is strongly typed and its
// enumerators are scoped. Code like `Color c = red;` and `Color c = 2;`
// will not compile.
enum class Color { red, blue, green };

// TrafficLight::red is different from Color::red
// We can also explicitly specify values.
enum class TrafficLight { green = 2, yellow = 1, red = 0 };

enum Foo { bar, baz, }

Color c = Color::red;
```

{{% cite Stroustrup2018-Ch2 %}}

An enum can be initialized from its underlying type (in this case,
`int`), e.g. `Color c2 {6}`, despite `6` not being a defined enumerator.
{{% cite Stroustrup2018-Ch2 %}}

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

Plain (or C-style) enums are entered in the same scope as the name of
their enum, and implicitly convert to their integer value, e.g.

```cpp
enum Color { red, green, blue };
int col = green;
```

{{% cite Stroustrup2018-Ch2 %}}

1. {{< citation
  id="Stroustrup2018-Ch2"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 2. User-Defined Types"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018" >}}

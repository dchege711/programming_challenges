---
authors:
- Stroustrup, Bjarne
date: 2022-06-04
domains:
- en.cppreference.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types-cpp/
title: User-Defined Types in C++
---

## The Motivation for User-Defined Types

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

Prefer user-defined types as they are easier to use, less error prone,
and typically as efficient as direct use of built-in types. {{% cite
Stroustrup2018-Ch2 %}}

{{% open-comment %}}

{{% cite Stroustrup2018-Ch2 %}} says that sometimes user-defined types
can be more efficient than built-in types. When would this be? Maybe
something like
[`std::vector<bool>`](https://en.cppreference.com/w/cpp/container/vector_bool),
which is possibly (implementation-defined) more space-efficient than a
`std::vector` (and thus an array) of `bool`?

{{% /open-comment %}}

## Structures

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

## Classes

A **class** is the language mechanism for separating the interface to a
type (to be used by all), and its implementation (which has access to
otherwise inaccessible data), e.g.

```cpp
class Vector {
 public:
  // The constructor is a special function that has the same name as the
  // class. It's guaranteed to be called when initializing objects of
  // this class. We no longer need `vector_init`.
  Vector(int s) : elem{new double[s]}, sz{s} {}

  double& operator[](int i) { return elem[i]; }
  int size() { return sz; }

 private:
  double* elem;   // pointer to elements on the free store
  int sz;         // the number of elements
};
```

{{% cite Stroustrup2018-Ch2 %}}

Notice that regardless of the number of elements, the `Vector` object
itself is always the same size. A fixed-size handle referring to a
variable amount of data "elsewhere" is a common technique for handling
varying amounts of information in C++. {{% cite Stroustrup2018-Ch2 %}}

There is no fundamental difference between a `struct` and a `class`; a
`struct` is simply a `class` with members public by default. {{% cite
Stroustrup2018-Ch2 %}}

{{% comment %}}

More in-depth discussion at [Classes in C++]({{< ref
"/computer-science/programming-challenges/language-concepts/classes/classes-in-cpp.md">}})

{{% /comment %}}

## Unions

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

## Enumerations

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

## References

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

---
authors:
- Stroustrup, Bjarne
date: 2022-06-04
domains:
- abseil.io
- chromium.googlesource.com
- en.cppreference.com
- isocpp.github.io
- www.chromium.org
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
Color c2 = 1; // error: invalid conversion from 'int' to 'Color' [-fpermissive]

enum CardColor { red, black }; // Error: "red" conflicts with a previous declaration
```

{{% cite Stroustrup2018-Ch2 %}}

The name can be omitted if it's not going to be used, e.g.

```cpp
enum { red, green, blue };
int col = red;
```

{{% cite cppReferenceEnumerationDeclaration %}}.

Unnamed enums are common in code written before alternative ways of
specifying integer constants, e.g.

```cpp
enum { red = 0xFF0000, scale = 4, is_signed = 1 };
```

But we can now have `constexpr int red = 0xFF0000;`, etc. {{% cite
cppCoreGuidelinesEnums %}}

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

Color c1 = Color::red;
Color c2 = red; // error: 'red' was not declared in this scope
Color c3 = 2; // error: cannot convert 'int' to 'Color' in initialization
```

{{% cite Stroustrup2018-Ch2 %}}

The scoping prevents collisions between enumerator names, e.g.
`Color::red` is different from and can co-exist with
`TrafficLight::red`. {{% cite Stroustrup2018-Ch2 %}}

C++20 added using-enum-declaration:

```cpp
enum class Fruit { orange, apple };
enum class Color { red, orange };

void f() {
    using enum Fruit;
    Fruit f = orange;

    using enum Color; // error: 'Color Color::orange' conflicts with a previous declaration
}
```

{{% cite cppReferenceEnumerationDeclaration %}}

## Underlying Type

By default `enum Color { red, blue }` is backed by an
implementation-defined integral type that can represent all enumerator
values. If no integral type can represent all the enumerator values, the
enumeration is ill-formed. A different underlying type can be specified.

```cpp
enum Color : unsigned char { red, blue };
enum Foo : bool { bar, baz, qux }; // error: enumerator value '2' is outside the range of underlying type 'bool'
enum Bar : float {}; // error: underlying type 'float' of 'Bar' must be an integral type

// To forward-declare an enum, we need to specify the underlying type.
// {{% cite cppCoreGuidelinesEnums %}}
enum Baz : char;
void f(Baz);

enum Baz : char {};
```

{{% cite cppReferenceEnumerationDeclaration %}}

{{% comment %}}

Built-in integral types are `bool`, `char`, `char8_t`, `char16_t`,
`char32_t`, `wchar_t`, `short`, `int`, `long` and `long long`. {{% cite
cppReferenceIsIntegral %}}

For space-efficiency, `enum Foo : char {};` is the most compact, as the
C++ Standard guarantees that `1 == sizeof(char) <= sizeof(short) <=
sizeof(int) <= sizeof(long) <= sizeof(long long)`. Note that
`sizeof(bool)` is implementation-defined, and may differ from `1`. {{%
cite cppReferenceFundamentalTypes %}}

{{% /comment %}}

{{% open-comment %}}

I've always considered `int`s as fast-enough. Would `char`s be
significantly faster? 64-bit machines are the norm, hence the typical
word size is 64 bits (or 8 bytes). `int` is typically 4 bytes and can
thus fit in a word. {{% cite cppReferenceSizeOfOperator %}} Are there
perf benefits for going below the word size?

{{% /open-comment %}}

Unscoped enums can implicit convert to their underlying type, but need a
static cast when being initialized from the underlying type, e.g.

```cpp
enum Color { red, green, blue };
int c1 = green; // Often undesirable as bugs may creep in.
Color c2 = static_cast<Color>(999); // OK, even if 999 is not a named enumerator
Color c3{2} // invalid conversion from 'int' to 'Color' [-fpermissive]
```

{{% cite cppReferenceEnumerationDeclaration %}}

One approach for validating that enumerators are known is defining a
function like `bool IsKnownEnumValue(MyEnum value)`, e.g. {{% cite
MojoDocs %}}, and then using it where the conversion from the integral
type to an enumerator is performed.

{{% comment %}}

Chromium defines `checked_cast<>()` for numeric types, which is like
`static_cast<>`, except that it triggers a crash at runtime, or a
compiler error if the conversion error can be detected at compile time.
{{% cite chromiumNumerics %}}

{{% /comment %}}

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

Given that a valid enumerator need not be named, use exhaustive
switch statements responsibly:

```cpp
enum Foo : unsigned char { bar, baz }; // Range [0, 255]

std::string ToStringBuggy(Foo foo) {
  switch (foo) {
      case Foo::bar: return "bar";
      case Foo::baz: return "baz";
  }
} // warning: control reaches end of non-void function [-Wreturn-type]

std::string ToString(Foo foo) {
  switch (foo) {
      case Foo::bar: return "bar";
      case Foo::baz: return "baz";
  }

  std::cerr << "Unknown Foo found.";
  return "kUnknownFoo";
}

int main() {
  std::cout << ToString(Foo{150}) << "\n"; // prints "kUnknownFoo"
  std::cout << ToStringBuggy(Foo{150}) << "\n"; // prints garbage, see https://godbolt.org/z/6Yq44MPc7
}
```

{{% cite abseilToTW147 %}}

{{% comment %}}

{{% cite abseilToTW147 %}} goes further than advocating for adding
explicit return statements (and appropriate error logs) for fall-through
cases. The objective of exhaustive switch statements is to ensure (via
the `-Wswitch` compiler flag) that all enumerators are explicitly
handled. This may not always be desirable, e.g. the owners of the enum
are different from the owners of the exhaustive switch statements, but
both code is in the same repository that the enum authors are blocked.

{{% /comment %}}

## Operators for Enumerations

By default, an `enum class` only has assignment, initialization, and
comparisons. However, it is a user-defined type, and so we can define
operators for it, e.g.

```cpp
enum class TrafficLight { red, green, yellow };

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

{{% comment %}}

For longer enums with many cases, but the enumerations take consecutive
values without holes, a `static_cast` helps with brevity:

```cpp
enum class Day { mon, tue, wed, thu, fri, sat, sun };

Day& operator++(Day& d) {
  return d = (d == Day::sun) ? Day::mon : Day{static_cast<int>(d)+1};
}
```

Beware of infinite recursions in the name of avoiding a `static_cast`,
e.g.

```cpp
enum class Day { mon, tue, wed, thu, fri, sat, sun };

Day& operator++(Day& d) {
  return d = (d == Day::sun) ? Day::mon : Day{++d}; // runtime error
}
```

{{% cite cppCoreGuidelinesEnums %}}

{{% /comment %}}

## Style

Avoid `ALL_CAPS` for enumerators, as that may conflict with macros, e.g.

```cpp
// web_colors.h (third party header)
#define RED   0xFF0000
// ... more definitions ...

// product_info.h
enum class Product_info { RED, PURPLE, BLUE };   // syntax error
```

{{% cite cppCoreGuidelinesEnums %}}

Special names can help validate the code via compiler plugins. For
example, Chromium has a clang plugin that checks that `kMaxValue` is
indeed the max value, e.g.

```cpp
enum class Foo {
  kOne, kTwo, kMaxValue = kOne,
};  // kMaxValue enumerator does not match max value 0 of other enumerators
```

{{% cite chromiumStyleChecker %}}

{{% open-comment %}}

How do I write a clang plugin? See [Clang Plugins — Clang 15.0.0git
documentation](https://clang.llvm.org/docs/ClangPlugins.html).

{{% /open-comment %}}

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

1. {{< citation
  id="cppReferenceIsIntegral"
  title="std::is_integral - cppreference.com"
  url="https://en.cppreference.com/w/cpp/types/is_integral"
  accessed="2022-06-05" >}}

1. {{< citation
  id="cppReferenceFundamentalTypes"
  title="Fundamental types - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/types"
  accessed="2022-06-05" >}}

1. {{< citation
  id="cppReferenceSizeOfOperator"
  title="sizeof operator - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/sizeof"
  accessed="2022-06-05" >}}

1. {{< citation
  id="chromiumNumerics"
  title="base/numerics/README.md > Conversion functions and StrictNumeric<> in safe_conversions.h"
  url="https://chromium.googlesource.com/chromium/src/+/master/base/numerics/README.md#conversion-functions-and-strictnumeric_in-safe_conversions_h"
  accessed="2022-06-05" >}}

1. {{< citation
  id="MojoDocs"
  title="Mojo docs (go/mojo-docs) - Mojo C++ Bindings API"
  url="https://chromium.googlesource.com/chromium/src/+/87.0.4280.88/mojo/public/cpp/bindings/README.md#versioned-enums"
  accessed="2022-06-05" >}}

1. {{< citation
  id="cppCoreGuidelinesEnums"
  title="C++ Core Guidelines > Enum: Enumerations"
  url="https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#enum-enumerations"
  accessed="2022-06-05" >}}

1. {{< citation
  id="abseilToTW147"
  title="abseil / Tip of the Week #147: Use Exhaustive switch Statements Responsibly"
  url="https://abseil.io/tips/147"
  accessed="2022-06-05" >}}

1. {{< citation
  id="chromiumStyleChecker"
  title="Chromium Style Checker Errors > Enumerator Max Values"
  url="https://www.chromium.org/developers/coding-style/chromium-style-checker-errors/#enumerator-max-values"
  accessed="2022-06-05" >}}

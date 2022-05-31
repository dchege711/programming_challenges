---
authors:
- Stroustrup, Bjarne
- Yorgey, Brent
date: 2022-05-30
domains:
- en.cppreference.com
- www.cis.upenn.edu
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/parameterization/templates-cpp/
title: Templates in C++
---

A **template** is a class or a function that we can parameterize with a
set of types or values. {{% cite Stroustrup2018-Ch6 %}}

## Parameterized Types

The [vector-of-doubles]({{< ref
"/computer-science/programming-challenges/language-concepts/classes/classes-in-cpp.md#containers"
>}}) can be generalized to a vector-of-anything type by making it a
template:

```cpp
// `template<typename T>` can be read as "for all types T". Older code
// uses `template<class T>`, which is equivalent.
template<typename T>
class Vector {
 public:
  explicit Vector(int s);
  ~Vector() { delete[] elem; }

  // ... copy and move operations ...

  T& operator[](int i);               // For non-const Vectors
  const T& operator[](int i) const;   // For const Vectors
  int size() const { return sz; }

 private:
  T* elem;
  int sz;
};
```

{{% cite Stroustrup2018-Ch6 %}}

Member functions are defined similarly, e.g.

```cpp
template<typename T>
Vector<T>::Vector(int s) {
  if (s < 0)
    throw Negative_size{};

  elem = new T[s];
  sz = s;
}
```

{{% cite Stroustrup2018-Ch6 %}}

A template plus a set of of template arguments is called an
**instantiation** or a **specialization**, e.g. `Vector<char>`. Late in
the compilation process, at **instantiation time**, code is generated
for each instantiation used in a program. Using templates incurs no
run-time overhead compared to hand-crafted code. {{% cite
Stroustrup2018-Ch6 %}}

{{% comment %}}

That templates are instantiated late in the compilation process may lead
to unintuitive compiler error messages. {{% cite
cppReferenceConstraintsAndConcepts %}}

{{% /comment %}}

### Constrained Template Arguments (C++20)

Constrained template arguments are useful when a template would only
make sense for template arguments that meet a certain criteria. This is
achieved using `template<Element T>`, which can be read as, "For all `T`
such that `Element(T)`". {{% cite Stroustrup2018-Ch6 %}}

`Element` is a predicate that checks whether `T` has all the properties
that the template class requires. Such a predicate is called a
**concept**. A template argument for which a concept is specified is
called a **constrained argument**, and a template for which an argument
is constrained is called a **constrained template**. Older code uses
unconstrained template arguments and leaves requirements to
documentation. {{% cite Stroustrup2018-Ch6 %}}

For example, suppose functions `f`, `g`, and `h` require that their
arguments be hashable. We can do:

```cpp
#include <string>
#include <cstddef>
#include <concepts>

template<typename T>
concept Hashable = requires(T a) {
  { std::hash<T>{}(a) } -> std::convertible_to<std::size_t>;
};

// One way of applying the Hashable constraint.
template<Hashable T>
void f(T) {}

struct Foo;

int main {
  f(std::string("s"));  // OK, std::string satisfies Hashable
  // f(Foo{});          // Error: Foo does not satisfy Hashable
}
```

{{% cite cppReferenceConstraintsAndConcepts %}}

{{% comment %}}

This is similar to Haskell's type classes. For example, the type of
`(==)` is `(==) :: Eq a => a -> a -> Bool`, which can be read as, "For
any type `a`, *as long as `a` is an instance of `Eq`*, `(==)` can take
two values of type `a` and return a `Bool`. {{% cite
cis194Spring2013TypeClasses %}}

However, there is a loose coupling that doesn't exist in Haskell's
version. In Haskell, a matching type `a` may be something of the form:

```hs
data Foo = F Int | G Char
  deriving (Eq)
```

{{% cite cis194Spring2013TypeClasses %}}

On the other hand, the C++ type that can be passed to `f` does not need
to reference the `Hashable` concept anywhere in its code.

{{% /comment %}}

### Value Template Arguments

In addition to to type arguments, a template can take value arguments
(which must be constant expressions), e.g.

```cpp
template<typename T, int N>
struct Buffer {
  // Convenience functions for accessng the template arguments.
  using value_type = T;
  constexpr int size() { return N; }

  T[N];
  // ...
};
```

{{% cite Stroustrup2018-Ch6 %}}

Value arguments are useful in many contexts. For example, `Buffer`
allows us to create arbitrarily sized buffers with no use of the free
store. {{% cite Stroustrup2018-Ch6 %}}

### Template Argument Deduction

Argument deduction can help reduce redundant typing, e.g.

```cpp
Vector v1 {1, 2, 3};  // Deduce v1's element type from the initializer list element type
Vector v2 = v1;       // Deduce v2's element type form v1's element type
Vector<int> v3(1);    // Need to be explicit as no element type is mentioned
```

{{% cite Stroustrup2018-Ch6 %}}

But deduction can also cause surprises, e.g.

```cpp
Vector<string> vs1 {"Hello", "World"};  // Vector<string>
Vector vs {"Hello", "World"};           // Deduces to Vector<const char*>
```

{{% cite Stroustrup2018-Ch6 %}}

When a template argument can't be deduced from the constructor
arguments, we can provide a **deduction guide**, e.g.

```cpp
// Template declaration
template<typename T>
class Vector2 {
 public:
  using value_type = T;

  Vector2(std::initializer_list<T>);  // Initializer-list constructor

  template<typename Iter>
    Vector2(Iter b, Iter e);          // [b:e) range constructor
};

// Additional deduction guide
template<typename Iter>
  Vector2(Iter, Iter) -> Vector2<typename Iter::value_type>;
```

{{% cite Stroustrup2018-Ch6 %}}

The user-defined deduction guide needs not be a template, e.g.

```cpp
template<class T> struct S {
  S(T);
};
S(char const*) -> S<std::string>;

S s{"Hello"}; // Deduced to S<std::string>
```

{{% cite cppReferenceCTAD %}}

The effects of deduction guides are often subtle, so limit their use;
prefer using concepts. {{% cite Stroustrup2018-Ch6 %}}

## References

1. {{< citation
  id="Stroustrup2018-Ch6"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 6. Templates"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018">}}

1. {{< citation
  id="cppReferenceConstraintsAndConcepts"
  title="Constraints and concepts (since C++20) - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/constraints"
  accessed="2022-05-30" >}}

1. {{< citation
  id="cis194Spring2013TypeClasses"
  author="Brent Yorgey"
  title="05-type-classes"
  url="https://www.cis.upenn.edu/~cis194/spring13/lectures/05-type-classes.html#type-classes"
  year="2013"
  accessed="2022-05-30" >}}

1. {{< citation
  id="cppReferenceCTAD"
  title="Class template argument deduction (CTAD) (since C++17) - cppreference.com"
  url="https://en.cppreference.com/w/cpp/language/class_template_argument_deduction"
  accessed="2022-05-30" >}}

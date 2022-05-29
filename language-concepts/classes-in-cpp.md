---
authors:
- Stroustrup, Bjarne
date: 2022-05-12
domains:
- clang.llvm.org
- en.cppreference.com
- en.wikipedia.org
- isocpp.org
- stackoverflow.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/classes-in-cpp/
title: Classes in C++
---

{{% open-comment %}}

{{% cite Stroustrup2018-Ch4 %}} quotes Doug McIlroy:

> Those types are not "abstract"; they are as real as `int` and `float`.

What is the context of this quote?

{{% /open-comment %}}

A class is a user-defined type provided to represent a concept in the
code of a program. Essentially, all language facilities beyond the
fundamental types, operators, and statements exist to help define better
class or to use them more conveniently. {{% cite Stroustrup2018-Ch4 %}}

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
Stroustrup2018-Ch4 %}}

For some concrete types, e.g. `std::vector` and `std::string`, the
representation may have pointers to data stored in the free store
(dynamic memory, heap). Such types can be considered resource handles
with carefully crafted interfaces. {{% cite Stroustrup2018-Ch4 %}}

The "representation is part of definition" property allows us to:

* Place objects of concrete types on the stack and in other objects.
* Refer to objects directly (and not just through pointers or
  references).
* Initialize objects immediately and completely.
* Copy and move objects.

{{% cite Stroustrup2018-Ch4 %}}

### An Arithmetic Type

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

{{% cite Stroustrup2018-Ch4 %}} notes that `complex`'s representation
has to be compatible with what Fortran provided 60 years ago.

Why is C++'s compatibility with Fortran important? [Intel says something
similar](https://www.intel.com/content/www/us/en/develop/documentation/onemkl-macos-developer-guide/top/language-specific-usage-options/mixed-language-programming-with-onemkl/using-complex-types-in-c-c.html).
It seems that mixing Fortran and C++ is commonplace, e.g. [IBM's
instructions](https://www.ibm.com/docs/en/xl-fortran-aix/15.1.2?topic=calls-mixing-fortran).

{{% /comment %}}

#### Inlining

Simple operations (such as constructors, `+=`, `imag`, etc.) must be
inlined (implemented without function calls in the generated machine
code) {{% cite Stroustrup2018-Ch4 %}}. {{% cite isoCPPInlineFunctions
%}} paints a more subtle picture of the effect of inlining on
performance. Inline functions might make the executable:

* Larger as the inlined functions get expanded in multiple places.
* Smaller if the expansion generates less code than the code to push/pop
  registers/parameters.
* Faster by removing unnecessary instructions, increasing cache hits
  because of improved locality.
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

#### Default Constructors

By defining a default constructor (one that can be invoked without
arguments), one eliminates the possibility of uninitialized variables of
that type. {{% cite Stroustrup2018-Ch4 %}} One can also have `complex()
= delete;`, which will cause a compiler error if the default constructor
gets selected. {{% cite cppReferenceDefaultConstructor %}}

#### Out-of-Class Operator Definitions

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
without affecting the caller's copy. {{% cite Stroustrup2018-Ch4 %}} Had
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

### Containers

A **container** is an object holding a collection of elements. It should
be simple to understand, establish useful invariants, and provide
range-checked access. {{% cite Stroustrup2018-Ch4 %}}

#### Motivation for the Destructor Mechanism

Although C++ provides an interface for plugging in a garbage collector,
aim to use the destructor pattern to reduce headache (e.g. GC's
availability not being guaranteed) {{% cite Stroustrup2018-Ch4 %}}:

```cpp
class Vector {
 public:
  Vector(int s) : elem{new double[s]}, sz{s} { // Ctor: acquires
    // resources from the free store.
    for (int i = 0; i != s; ++i) // Initialize elements
      elem[i] = 0;
  }

  ~Vector() { // Destructor: release resources
    delete[] elem;
  }

  double& operator[](int i);
  int size() const;

 private:
  double* elem; // Points to an array of sz doubles
  int sz;
};
```

The technique of acquiring resources in a constructor and releasing them
in a destructor, known as **Resource Acquisition Is Initialization
(RAII)**, allows us to avoid naked `new` and `delete` operations in
general code. {{% cite Stroustrup2018-Ch4 %}}

In RAII, resource acquisition must succeed for initialization to
succeed. The resource is guaranteed to be held between when
initialization finishes and finalization starts (holding the resources)
is a class invariant, and to be held only when the object is alive. If
there are no object leaks, then there are no resource leaks. {{% cite
wikiRAII %}}

RAII only works for resources acquired and released by stack-allocated
objects, where there is a well-defined static object lifetime. Heap
based objects must be deleted along all possible execution paths to
trigger their destructor. {{% cite wikiRAII %}}

In C++, stack unwinding (popping one or more frames off the stack to
resume execution elsewhere in the program) is only guaranteed to happen
if the exception is caught somewhere. If it's not caught, `terminate` is
called and stack unwinding at this point is implementation-defined. The
OS usually releases program resources at termination, so it usually
works out. {{% cite wikiRAII %}}

#### Initializing Containers

```cpp
class Vector {
  public:
    Vector(std::initializer_list<double>);
    // ...
    void push_back(double); // Useful for an arbitrary number of elements.
    // ...
};
```

The `std::initializer_list` is a standard-library type know to the
compiler. Whenever we use a `{}`-list, the compiler creates an object of
type `initializer_list` to give to the program, e.g. `Vector v = {1, 2,
3, 4, 5}`. The initializer-list constructor may be defined as: {{% cite
Stroustrup2018-Ch4 %}}

```cpp
Vector::Vector(std::initializer_list<double> lst)
    : elem{new double[lst.size()]}, sz{static_cast<int>(lst.size())} {
  copy(lst.begin(), lst.end(), elem) // Copy from lst into elem
}
```

An object of type `std::initializer_list<T>` is a lightweight proxy
object that provides access to an array of objects of type `const T`.
Copying a `std::initializer_list` does not copy the underlying objects.
{{% cite cppReferenceInitializerList %}}

The `static_cast` is used to convert from the unsigned `size_t` returned
by `initializer_list::size()`. We're assuming that a handwritten list
won't have more elements than the largest integer. `*cast`s should be
used sparingly as they are error-prone. {{% cite Stroustrup2018-Ch4 %}}

{{% comment %}}

Containers may define their `container::size_type` as they wish. It may
not always be `std::size_t`. That said, `std::size_t` can store the
maximum size of a theoretically object of any type (including array). A
type whose size cannot be represented by `std::size_t` is ill-formed.
{{% cite cppReferenceSizeT %}}

Programs using other types, e.g. `unsigned int`, for array-indexing and
loop counting may fail on 64-bit systems when the index exceeds
`UINT_MAX` or if the program relies on 32-bit modular arithmetic. {{%
cite cppReferenceSizeT %}}

{{% /comment %}}

## Abstract Types

An abstract type is a type that completely insulates a user from
implementation details. The interface is decoupled from the
representation, and there are no genuine local variables. {{% cite
Stroustrup2018-Ch4 %}}

```cpp
// Container is an abstract class because it has a pure virtual
// function.
class Container {
 public:
  // It is common for abstract classes to not have a constructor. After
  // all, there is no data to initialize.

  // The destructor is declared virtual so that derived classes can
  // define implementations. Someone destroying a Container through a
  // pointer has no idea what resources are owned by its implementation.
  virtual ~Container() {}

  // The "= 0" syntax says the function is pure virtual. Some class
  // derived from Container MUST define the function.
  virtual double& operator[](int) = 0;

  virtual int size() const = 0;
};
```

{{% cite Stroustrup2018-Ch4 %}}

An abstract class cannot be instantiated; we can't do `Container c;` -
we don't even know the size of `Container`. {{% cite Stroustrup2018-Ch4
%}}

### Using an Abstract Class

For `Container` to be useful, we need a class that implements the
functions required by the interface:

```cpp
// ": public" can be read as "is derived from".
// Vector_container is said to be derived from class Container, or a
// subclass.
// Container is said to be a base class of Vector_container, or a
// superclass.
// The derived class is said to inherit members from its base class, and
// so the use of base and derived classes is commonly referred to as
// inheritance.
class Vector_container : public Container {
 public:
  Vector_container(int s) : v(s) {}

  // `-Winconsistent-missing-destructor-override` warns about the
  // missing "override" below.
  // {{% cite clangDiagnosticsMissingOverrides %}}
  ~Vector_container() {}

  // The use of "override" is optional, but being explicit helps the
  // compiler catch mistakes.
  //
  // `-Winconsistent-missing-override`, which warns if the "override"
  // keyword is not specified when it should, is enabled by default.
  // {{% cite clangDiagnosticsMissingOverrides %}}
  double& operator[](int i) override { return v[i]; }
  int size() const override { return v.size(); }

 private:
  Vector v;
};
```

`Vector_container` can be initialized and referred to as a `Container`,
i.e. `Container* p = new Vector_container(10)`. {{% cite
Stroustrup2018-Ch4 %}}

A `Container` can be used like this:

```cpp
void use(Container& c) {
  const int sz = c.size();
  for (int i = 0; i != sz; ++i)
    std::cout << c[i] << '\n';
}
```

`use(Container&)` has no idea if its argument is a `Vector_container`,
or some other kind of container, and it doesn't need to know. If the
implementation of `Vector_container` changed, `use(Container&)` need not
be re-compiled. The flip side of this flexibility is that `Container`
objects must be manipulated through pointers or references. {{% cite
Stroustrup2018-Ch4 %}}

### The Virtual Function Table

A `Container` object must contain information to allow it to select the
right function to call at runtime. A common implementation is a virtual
function table, or simply the `vtbl`. Each class with virtual functions
has its own `vtbl`. {{% cite Stroustrup2018-Ch4 %}}

{{< figure
  src="/img/computer-science/programming-challenges/language-concepts/cpp/virtual-function-table.jpg"
  caption=`Graphical representation of a virtual function table. Source: Stroustrup2018-Ch4.`>}}

The implementation of the caller needs only to know the location of the
pointer to the `vtbl` in a `Container`, and the index used for each
virtual function. The virtual call mechanism can be made almost as
efficient as the "normal function call mechanism (within 25%). {{% cite
Stroustrup2018-Ch4 %}}

{{% open-comment %}}

[Herb Stutter recommends](http://www.gotw.ca/publications/mill18.htm):

* Prefer to make interfaces non-virtual, using Template Method.
* Prefer to make virtual functions private.
* Only if derived classes need to invoke the base implementation of a
  virtual function, make the class protected.
* A base destructor should be either public and virtual, or protected
  and non-virtual.

Review the arguments for the above recommendations.

{{% /open-comment %}}

### Protected and Private Inheritance

There is also protected inheritance, and private inheritance. It is all
about access to the inherited members. From {{% cite
soPublicPrivateAndProtectedInheritance %}}:

```cpp
class A {
 public:
  int x;
 protected:
  int y;
 private:
  int z;
};

class B : public A {
  // x is public; y is protected; z is not accessible from B.
};

class C : protected A {
  // x is protected; y is protected; z is not accessible from C.
};

class D : private A { // 'private' is default for classes.
  // x is private; y is private; z is not accessible from D
}
```

Notice that derived classes cannot expose inherited members beyond the
access level defined in the base class. But a derived class can hide the
inherited members.

## References

1. {{< citation
  id="Stroustrup2018-Ch4"
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

1. {{< citation
  id="wikiRAII"
  title="Resource acquisition is initialization - Wikipedia"
  url="https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization"
  accessed="2022-05-28" >}}

1. {{< citation
  id="cppReferenceInitializerList"
  title="std::initializer_list - cppreference.com"
  url="https://en.cppreference.com/w/cpp/utility/initializer_list"
  accessed="2022-05-28" >}}

1. {{< citation
  id="cppReferenceSizeT"
  title="std::size_t - cppreference.com"
  url="https://en.cppreference.com/w/cpp/types/size_t"
  accessed="2022-05-28" >}}

1. {{< citation
  id="clangDiagnosticsMissingOverrides"
  title="Diagnostic flags in Clang — Clang 15.0.0 git documentation > Winconsistent-missing-*override"
  url="https://clang.llvm.org/docs/DiagnosticsReference.html#winconsistent-missing-override"
  url_2="https://clang.llvm.org/docs/DiagnosticsReference.html#winconsistent-missing-destructor-override"
  accessed="2022-05-28" >}}

1. {{< citation
  id="soPublicPrivateAndProtectedInheritance"
  title="What is the difference between public, private, and protected inheritance in C++? - Stack Overflow"
  url="https://stackoverflow.com/a/1372858/7812406"
  accessed="2022-05-28" >}}

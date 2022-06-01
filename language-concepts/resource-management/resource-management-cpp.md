---
authors:
- Stroustrup, Bjarne
date: 2022-05-31
domains:
- abseil.io
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/resource-management/resource-management-cpp/
title: Resource Management in C++
---

## Resource Handles

{{% priors %}}

RAII is also covered in [Classes in C++ > Motivation for the Destructor
Mechanism]({{< ref
"/computer-science/programming-challenges/language-concepts/classes/classes-in-cpp#motivation-for-the-destructor-mechanism"
>}})

{{% /priors %}}

The constructor/destructor pattern enables objects defined in a scope to
release the resources during exit from the scope, even when exceptions
are thrown. All standard-library containers, e.g. `std::vector`, are
implemented as resource handles. {{% cite Stroustrup2018-Ch13 %}}

## `std::unique_ptr` and `std::shared_ptr`

These "smart pointers" are useful in managing objects that are allocated
on the free store (as opposed to those allocated on the stack). {{% cite
Stroustrup2018-Ch13 %}}

A `unique_ptr` ensures that an object is properly destroyed when the
`unique_ptr` goes out of scope, e.g.

```cpp
void f(int i, int j) {
  X* p = new X;
  std::unique_ptr<X> sp {new X};

  if (i < 99) throw Z{};  // p does not get deleted when we exit f
  if (j < 77) return;     // p does not get deleted when we exit f

  // ... use p and sp ...
  delete p;
} // sp will delete the managed X

// Admittedly, the following scheme is cleaner.
void f(int i, int j) {
  X x; // Allocated on the stack. Will be cleaned up on exiting f.
  // ...
}
```

{{% cite Stroustrup2018-Ch13 %}}

When pointer semantics are needed, e.g. passing free-store allocated
objects in and out of functions, a `unique_ptr` has no space or time
overhead compared to correct use of a built-in pointer:

```cpp
std::unique_ptr<X> make_X(int i) {
  // ... check i, etc. ...
  return std::unique_ptr<X>{new X{i}};
}
```

{{% open-comment %}}

Why not use a `std::optional`?

{{% /open-comment %}}

{{% cite Stroustrup2018-Ch13 %}}

A `shared_ptr` is used to model shared ownership. The object is deleted
when the last of its `shared_ptr`s is destroyed, e.g.

```cpp
void f(std::shared_ptr<fstream>);
void g(std::shared_ptr<fstream>);

void user(const std::string& name, base::openmode mode) {
  std::shared_ptr<fstream> fp {new fstream(name, mode)};
  if (!*fp) throw No_file{};

  f(fp);
  g(fp);
}
```

{{% cite Stroustrup2018-Ch13 %}}

Although `shared_ptr`s are neither cost-free nor exorbitantly expensive,
they make the lifetime of the shared object hard to predict, e.g. `f()`
spawns a task that holds a copy of `fp` and outlives `user`. {{% cite
Stroustrup2018-Ch13 %}}

To reduce verbosity, the use of `new`, and possible bugs (e.g. passing
a pointer of something that's not on the free-store), prefer to use
`std::make_unique` and `std::make_shared`, e.g.

```cpp
struct S { int i; std::string s; double d; };

auto p1 = std::make_unique<S>(1, "Ankh Morpork", 4.65);

// Furthermore, `make_shared` is more efficient because it does not need
// a separate allocation for the use count.
auto p2 = std::make_shared<S>(2, "Oz", 7.62);
```

{{% cite Stroustrup2018-Ch13 %}}

Prefer using resource handles, and only use "smart pointers" when
pointer semantics are needed:

* When we share an object, we need pointers (or references) to refer to
  the shared object.
* When we refer to [a polymorphic object]({{< ref
  "/computer-science/programming-challenges/language-concepts/classes/classes-in-cpp#abstract-types"
  >}}), we need a pointer (or a reference) because we don't know the
  exact type of the object referred to.
* When using legacy APIs that return raw pointers to owned data. {{%
  cite abseilToTW187 %}}

In particular, a pointer is not need to return a collection of objects
from a function. A resource handle suffices. {{% cite
Stroustrup2018-Ch13 %}}

When ownership is not transferred, a `std::unique_ptr` is likely not
needed. Furthermore, we'd have cognitive overhead for "what if the
pointer is empty?", and performance implications for the heap-allocated
object (less likely to be in CPU cache). {{% cite abseilToTW187 %}}

## `std::move` and `std::forward`

A `unique_ptr` is the sole owner of an object, and therefore it cannot
be copied. It must be explicitly moved if needed elsewhere. {{% cite
Stroustrup2018-Ch13 %}}

Think of `std::move` as `rvalue_cast`. It does not move anything.
Instead, it casts its argument to an rvalue reference, thereby saying
that the argument will not be used again and therefore may be moved, e.g.

```cpp
template <typename T>
void swap(T& a, T& b) {
  T tmp {std::move(a)};   // The T constructor sees an rvalue and moves
  a = std::move(b);       // The T assignment sees an rvalue and moves
  b = std::move(tmp);     // The T assignment sees an rvalue and moves
}
```

{{% cite Stroustrup2018-Ch13 %}}

Unless you can demonstrate significant and necessary perf improvements,
avoid uses of `std::move` that leave behind a moved-from object that
may get used again. The state of a moved-from object is generally
unspecified, but STL moved-from objects are in a state where they can
be destroyed and assigned to, and furthermore, STL containers are in an
"empty" state. {{% cite Stroustrup2018-Ch13 %}}

`std::forward` is useful when transmitting a set of arguments on to
another function without changing anything. Once you forward an object,
don't use it again (including forwarding it a second time).
`std::forward` is also a bit more sophisticated than `std::move` when
handling lvalue and rvalue subtleties.

```cpp
template <typename T, typename... Args>
unique_ptr<T> make_unique(Args&&... args) {
  return unique_ptr<T>{new T{std::forward<Args>(args)...}};
}
```

{{% cite Stroustrup2018-Ch13 %}}

## References

1. {{< citation
  id="Stroustrup2018-Ch13"
  title="A Tour of C++ (Second Edition)"
  sub-title="Chapter 13. Utilities"
  author="Bjarne Stroustrup"
  isbn="978-0-13-499783-4"
  year="2018">}}

1. {{< citation
  id="abseilToTW187"
  title="abseil / Tip of the Week #187: std::unique_ptr Must Be Moved"
  url="https://abseil.io/tips/187#fn:unique"
  accessed="2022-05-31" >}}

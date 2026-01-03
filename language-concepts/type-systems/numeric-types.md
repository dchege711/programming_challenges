---
title: "Remarks on Numeric Types"
date: 2026-01-02
---

## Inconveniences of Unsigned Types in .NET

Languages like C++ come with convenient support for unsigned integer types where
non-negative values make sense. `std::size_t` is the unsigned integer type of
the `sizeof` and `alignof` operators. `std::size_t` is also used when indexing
C++ containers. {{% cite sizeTCpp %}}

In contrast, .NET tends to use signed integer types even where unsigned ones
would be more intuitive, e.g.,

* `List<T>.Count` returns an `Int32` {{% cite ListTCount %}}
* Indexing into a `List<T>` takes in an `Int32` index {{% cite ListTIndex %}}

.NET is language independent. Languages like C#, F#, and Visual Basic target
.NET implementations. The Common Language Specification (CLS) is the set of
features that are common to all the languages. Of relevance is the fact that
`Byte`, `Int16`, `Int32`, and `Int64` are the only CLS-compliant integer types.
{{% cite dotNetCLS %}} {{% cite SO10041251 %}}

{{% comment %}}

The use of signed integer types over unsigned ones also appears in other areas
like LINQ helpers, e.g., `Enumerable.Sum` has overloads for `Int32` and `Int64`
but not the unsigned versions. {{% cite Enumerable.Sum %}}

{{% /comment %}}

## References

1. {{< citation
  id="sizeTCpp"
  title="std::size_t - cppreference.com"
  url="https://en.cppreference.com/w/cpp/types/size_t.html"
  accessed="2026-01-02" >}}

https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1.count?view=net-10.0

1. {{< citation
  id="ListTCount"
  title="List<T>.Count Property (System.Collections.Generic) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1.count?view=net-10.0"
  accessed="2026-01-02" >}}

1. {{< citation
  id="ListTIndex"
  title="List<T>.Item[Int32] Property (System.Collections.Generic) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1.item?view=net-10.0"
  accessed="2026-01-02" >}}

1. {{< citation
  id="dotNetCLS"
  title="Language independence and language-independent components - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/language-independence"
  accessed="2026-01-02" >}}

1. {{< citation
  id="SO10041251"
  title="c# - Signed vs. unsigned integers for lengths/counts - Stack Overflow"
  url="https://stackoverflow.com/a/10041251"
  accessed="2026-01-02" >}}

1. {{< citation
  id="Enumerable.Sum"
  title="Enumerable.Sum Method (System.Linq) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.sum?view=net-10.0"
  accessed="2026-01-02" >}}
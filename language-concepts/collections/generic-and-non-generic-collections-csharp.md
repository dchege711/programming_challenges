---
date: 2025-09-06
domains:
- github.com
- learn.microsoft.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/collections/generic-and-non-generic-collections-csharp/
title: Generic and Non-Generic Collection Interfaces in C#
---

There are two main types of collections: non-generic (`ICollection`) and generic
(`ICollection<T>`). {{% cite CollectionsAndDataStructures %}}

Non-generic collections only exist because .NET did not originally have generic
data types. They shouldn't be used because:

* They are untyped at compile time. The frequent casting from `object` and the
  actual type is error-prone; it's easy to put the wrong type in the wrong
  collection.
* Value types need to be boxed as `object`, e.g., `List<int>` stores its data in
  an `int[]`, which is more performant than using `object[]` as that requires
  boxing.

{{% cite PlatformCompatDE0006 %}}

## Non-Generic Collections (`ICollection`)

{{% cite ICollection %}}

`ICollection` extends `IEnumerable`.

### Properties of `ICollection`

{{% cite ICollection %}}

`Count` gets the number of elements contained in the `ICollection`.

{{% comment %}}

This is different from the capacity, which is the number of elements that a
collection can contain. For most collections, when the current capacity is
reached, the memory is reallocated, and the elements are copied from the old
collection to the new one. To avoid poor performance from multiple
reallocations, set the initial capacity to the estimated size of the collection.
{{% cite CollectionsAndDataStructures %}}

{{% /comment %}}

`IsSynchronized` is `true` if access to the `ICollection` is thread-safe.

`SyncRoot` is an object that can be used to synchronize the `ICollection`.

### Methods of `ICollection`

{{% cite ICollection %}}

`CopyTo` copies the elements of the collection to an `Array` starting at a
particular `Array` index.

`GetEnumerator` is an enumerator that iterates through a collection.

### Extension Methods of `ICollection`

{{% cite ICollection %}}

`Cast<TResult>` casts the elements of an `IEnumerable` to `TResult`.

{{% open-comment %}}

I still don't understand the rationale. In []({{< ref
"/computer-science/programming-challenges/advent-of-code/2024/AoC2024/06-guard-gallivant/06-guard-gallivant"
>}}), `Cast` helped me iterate over a multi-dimensional array. Why wouldn't the
multi-dimensional array expose LINQ methods instead of needing `Cast`?

{{% /open-comment %}}

`OfType<TResult>` filters the `TResult` instances in an `IEnumerable`.

`AsParallel` enables parallelization of a query.

`AsQueryable` converts an `IEnumerable` into an `IQueryable`.

## Generic Collections (`ICollection<T>`)

{{% cite IGenericCollection %}}

`ICollection<T>` extends `IEnumerable<T>`.

### Properties of `ICollection<T>`

{{% cite IGenericCollection %}}

Compared to `ICollection`, keeps `Count`, but lacks `IsSynchronized` and
`SyncRoot`.

`IsReadOnlye` is `true` if the `ICollection<T>` is read-only.

### Methods of `ICollection<T>`

{{% cite IGenericCollection %}}

Compared to `ICollection`, keeps `CopyTo` and `GetEnumerator`.

`Add`, `Clear`, `Contains`, and `Remove`.

### Extension Methods of `ICollection<T>`

{{% cite IGenericCollection %}}

Compared to `ICollection`, keeps `Cast`, `OfType`, `AsParallel`, and
`AsQueryable`.

#### Conversion to Other Collection Types

{{% cite IGenericCollection %}}

`ToFrozen*` for `Dictionary` and `Set` create immutable, read-only collections
that are optimized for fast lookup and enumeration. {{% cite
System.Collections.Frozen %}}

`To*` and `ToImmutable*` for `Array`, `Dictionary`, `HashSet`, `List`,
`SortedDictionary`, and `SortedSet`. Additionally, `ToLookup` creates a
collection of keys mapped to one or more values {{% cite Lookup %}}.

#### Filtering Elements in a Sequence

{{% cite IGenericCollection %}}

`All` and `Any` test elements in the sequence against a predicate.

`Where` filters a sequence of values based on a predicate.

`Distinct` and `DistinctBy` produce distinct elements from a sequence.

#### Fetching and Adding Scalars

{{% cite IGenericCollection %}}

`Single` returns the only element that satisfies the predicate and `throw`s if
more than one such element exists. `SingleOrDefault` is like `Single` but
returns a default value if no such element exists.

`Prepend` and `Append` add a value to the beginning and end of a sequence,
respectively.

`First`, `FirstOrDefault`, `Last`, `LastOrDefault`, `ElementAt` and
`ElementAtOrDefault` for index-based selection.

#### Operating on Multiple Sequences

{{% cite IGenericCollection %}}

`Concat` concatenates two sequences.

Set operations between two sequences: `Except` and `ExceptBy` for the
difference; `Intersect` and `IntersectBy` for the intersection; `Union` and
`UnionBy` for the union.

`Join` and `GroupJoin` correlates elements of two sequences by a specified key.

`SequenceEqual` tests whether two sequences are equal.

#### Projecting Elements to a Different Type

{{% cite IGenericCollection %}}

`Select` projects each element into a new form. `SelectMany` flattens the
resulting sequences into one sequence.

`AsEnumerable` changes the compile-time type of `source` from a type that
implements `IEnumerable<T>` to `IEnumerable<T>` itself. Sample use-case:
foregoing a custom implementation of `Where()`. {{% cite Enumerable.AsEnumerable
%}}

#### Reducing a Sequence

{{% cite IGenericCollection %}}

`Aggregate` and `AggregateBy` apply an accumulator function, akin to `reduce` in
other languages. There are mathematical aggregations such as `Average`, `Max`,
`MaxBy`, `Min`, `MinBy`, and `Sum`.

`Count` and `CountBy` count the number of elements that satisfy a predicate.
`LongCount` is similar but returns an `Int64`. `TryGetNonEnumeratedCount`
attempts to get the count without forcing an enumeration.

#### Iterating Through a Sequence

{{% cite IGenericCollection %}}

`Chunk` splits the elements of a sequence into chunks of size at most `size`.

`DefaultIfEmpty` returns the sequence if non-empty and otherwise returns a
singleton collection with the defined default value.

`GroupBy` groups the elements of a sequence by a specified key.

`Index` returns an enumerable that incorporates the element's index into a
tuple.

`Order`, `OrderDescending`, `OrderBy`, and `OrderByDescending` sort the elements
of a sequence.

`Reverse` inverts the order of elements in a sequence.

`Skip` and `SkipLast` bypass a specified number of elements in the sequence and
returns the remaining ones. `SkipWhile` checks a predicate instead of counting.
`Take`, `TakeLast`, and `TakeWhile` are inverses.

`Zip` produces a sequence of tuples with elements from two/three sequences, or
two sequences and a computed sequence.

## References

1. {{< citation
  id="CollectionsAndDataStructures"
  title="Collections and Data Structures - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/collections/"
  accessed="2025-09-06" >}}

1. {{< citation
  id="PlatformCompatDE0006"
  title="platform-compat/docs/DE0006.md at master · dotnet/platform-compat"
  url="https://github.com/dotnet/platform-compat/blob/master/docs/DE0006.md"
  accessed="2025-09-06" >}}

1. {{< citation
  id="ICollection"
  title="ICollection Interface (System.Collections) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.icollection?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="IGenericCollection"
  title="ICollection<T> Interface (System.Collections.Generic) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.icollection-1?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="System.Collections.Frozen"
  title="System.Collections.Frozen Namespace | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.frozen?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="Lookup"
  title="Lookup<TKey,TElement> Class (System.Linq) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.linq.lookup-2?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="Enumerable.AsEnumerable"
  title="Enumerable.AsEnumerable<TSource>(IEnumerable<TSource>) Method (System.Linq) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.asenumerable?view=net-9.0#system-linq-enumerable-asenumerable-1(system-collections-generic-ienumerable((-0)))"
  accessed="2025-09-06" >}}

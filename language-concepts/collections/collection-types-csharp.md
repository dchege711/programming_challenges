---
date: 2025-09-06
domains:
- learn.microsoft.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/collections/collection-types-csharp/
title: Collection Types in C#
---

## Choosing a Collection Type

In summary, the use cases are:

* `Dictionary<TKey, TValue>` for quick look-up by key.
* `List<T>` for accessing items by index.
* `Queue<T>` for first-in-first-out (FIFO) access where the element is
  discarded after use.
* `Stack<T>` for last-in-first-out (LIFO) access where the element is discarded
  after use.
* `LinkedList<T>` for sequential access from either the head or the tail.
* `ObservableCollection<T>` to receive notifications when items are
  removed/added.
* `SortedList<TKey, TValue>` for a collection sorted by keys and accessible by
  either key or index.
* `HashSet<T>` and `SortedSet<T>` for a set for mathematical functions.
* `KeyedCollection<TKey, TItem>` for items that have keys embedded in them; a
  hybrid between `IList<T>` and `IDictionary<TKey, TValue>`.

{{% cite CollectionsAndDataStructures %}} {{% cite SelectingACollection %}}

## `Array` and `ArrayList`

`Array`, e.g., `int[]`, is the base class for language implementations that
support arrays. It's zero-indexed by default, but can have any lower bound. It
can have at most 32 dimensions. Unlike classes in `System.Collections`, an
`Array` has a fixed capacity; to increase capacity, you must manually create a
new `Array` with the required capacity and move things over. At runtime, single
dimensional arrays implement `IList<T>`, `ICollection<T>`, `IEnumerable<T>`,
`IReadOnlyList<T>`, and `IReadOnlyCollection<T>`. Note that casting an array to
one of these interfaces and then trying to add/remove elements will throw
`NotSupportedException`. {{% cite Array %}}

`ArrayList` implements `IList` using an `Array` whose size is dynamically
increased as required. The generic counterpart is `List<T>`. {{% cite ArrayList
%}}

## Immutability

`System.Collections.Generic`'s `IReadOnlyCollection<T>` interface is derived by
interfaces like `System.Collections.Generic.IReadOnly*<T>` and
`System.Collections.Immutable.Immutable*<T>`. {{% cite
GenericIReadOnlyCollection %}}

`IReadOnlyList<T>` is read-only in the number and order of elements; the
contents of the contained elements are not guaranteed to be read-only. {{% cite
GenericIReadOnlyList %}} Compare this to `ImmutableList<T>` where any mutation
operation, even `ImmutableList<T>.SetItem(Int32, T)`, returns a new
`ImmutableList<T>`. {{% cite GenericImmutableList %}}

When building immutable collections, call `CreateBuilder` to get a `Builder`
object so that you can batch operations in a mutable state. When all mutations
have been completed, call `ToImmutable()` to "freeze" all nodes and create an
immutable collection. {{% cite UsingGenericCollections %}}

Enumerating an `ImmutableList<T>` is less efficient than enumerating a `List<T>`
because `ImmutableList<T>`'s indexer is \\(\mathcal{O}(log(n))\\) for traversing
the underlying binary tree. {{% cite CollectionsAndDataStructures %}}

## Thread-Safety

Collections from `System.Collections.Concurrent` are type-safe, thread-safe, and
scalable, e.g., `BlockingCollection<T>`, `ConcurrentDictionary<TKey, TValue>`,
`ConcurrentQueue<T>`, `ConcurrentStack<T>`, `ConcurrentBag<T>`, and
`IProducerConsumerCollection<T>`. {{% cite ThreadSafeCollections %}}

Collections from the `System.Collections.Immutable` namespace are inherently
thread-safe because operations act on copies of the data. {{% cite
CollectionsAndDataStructures %}}

## String Considerations

By default, comparisons and sorts in `System.Collections` are culture-aware,
which may lead to inconsistent comparisons. To remedy this, provide a
`StringComparer`, e.g., `StringComparer.OrdinalIgnoreCase` {{% cite
StringComparer %}}.

`System.Collections.Specialized` contains strongly typed collections like
`StringCollection`, `StringDictionary`, and `NameValueCollection`. {{% cite
System.Collections.Specialized %}}

{{% open-comment %}}

What optimizations do they have over `List<String>` and `Dictionary<String,
String>`?

{{% /open-comment %}}

## Sorted Collections

`SortedList<TKey, TValue>` and `SortedDictionary<TKey, TValue>` both have
\\(\mathcal{O}(log(n))\\) retrieval. However, `SortedList<TKey, TValue>` uses
less memory. `SortedList<TKey, TValue>` has \\(\mathcal{O}(n)\\) insertion and
removal for unsorted data compared to `SortedDictionary<TKey, TValue>`'s
\\(\mathcal{O}(log(n))\\). However, if the list is populated at once from sorted
data, then `SortedList<TKey, TValue>` is faster. The collections returned by
`Keys` and `Values` properties for `SortedList<TKey, TValue>` are just wrappers
around the internal arrays and thus do not need regeneration. {{% cite
GenericSortedList %}}

## References

1. {{< citation
  id="CollectionsAndDataStructures"
  title="Collections and Data Structures - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/collections/"
  accessed="2025-09-06" >}}

1. {{< citation
  id="SelectingACollection"
  title="Selecting a Collection Class - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/collections/selecting-a-collection-class"
  accessed="2025-09-06" >}}

1. {{< citation
  id="Array"
  title="Array Class (System) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.array?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="ArrayList"
  title="ArrayList Class (System.Collections) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.arraylist?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="System.Collections.Specialized"
  title="System.Collections.Specialized Namespace | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.specialized?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="GenericSortedList"
  title="SortedList<TKey,TValue> Class (System.Collections.Generic) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.sortedlist-2?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="UsingGenericCollections"
  title="When to Use Generic Collections - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/collections/when-to-use-generic-collections"
  accessed="2025-09-06" >}}

1. {{< citation
  id="StringComparer"
  title="StringComparer Class (System) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.stringcomparer?view=net-9.0"
  accessed="2025-09-06" >}}

1. {{< citation
  id="ThreadSafeCollections"
  title="Thread-Safe collections - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/collections/thread-safe/"
  accessed="2025-09-06" >}}

1. {{< citation
  id="GenericIReadOnlyCollection"
  title="IReadOnlyCollection<T> Interface (System.Collections.Generic) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ireadonlycollection-1?view=net-9.0"
  accessed="2025-09-13" >}}

1. {{< citation
  id="GenericIReadOnlyList"
  title="IReadOnlyList<T> Interface (System.Collections.Generic) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ireadonlylist-1?view=net-9.0"
  accessed="2025-09-13" >}}

1. {{< citation
  id="GenericImmutableList"
  title="ImmutableList<T> Class (System.Collections.Immutable) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.collections.immutable.immutablelist-1?view=net-9.0"
  accessed="2025-09-13" >}}

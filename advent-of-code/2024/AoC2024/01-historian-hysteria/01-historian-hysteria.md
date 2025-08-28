---
date: 2025-06-25
domains:
- adventofcode.com
- learn.microsoft.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2024/AoC2024/01-historian-hysteria/01-historian-hysteria/
title: 'AoC 2024 Day 01: Historian Hysteria'
---

{{< citation
  id="AoC2024Day01"
  title="Day 1 - Advent of Code 2024: Historian Hysteria"
  url="https://adventofcode.com/2024/day/1"
  accessed="2025-06-25" >}}

## Data

There's just one problem: by holding the two lists up **side by side**, it
quickly becomes clear that the lists aren't very similar. Maybe you can help The
Historians reconcile their lists?

For example:

```txt
3 4
4 3
2 5
1 3
3 9
3 3
```

To parse the input:

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/01-historian-hysteria/HistorianHysteria.Parse.cs"
  highlight="cs"
  id="HistorianHysteria.Parse.cs">}}

Creating a `static` class is basically the same as creating a class that
contains only `static` members and a `private` constructor. A `private`
constructor prevents the class from being instantiated. The advantage of using a
`static` class is that the compiler can check to make sure that no instance
members are accidentally added. The compiler guarantees that instances of this
class can't be created. Static classes are `sealed` and therefore can't be
inherited. They can't inherit from any class or interface except `Object`.
Static classes can't contain an instance constructor. However, they can contain
a static constructor. Non-static classes should also define a static constructor
if the class contains static members that require non-trivial initialization.
{{% cite StaticClasses %}}

Using source generation provides the most efficient regex code.
`RegexOptions.Compiled` improves on bare `new Regex()`, e.g., "match the input
character at the current position against 'a' or 'c'" vs. "match the input
character at the current position against the set specified in this set
description". However, `RegexOptions.Compiled` is costly to construct and uses
reflection. `GeneratedRegex` sidesteps these limitations while still being
efficient. {{% cite RegexSourceGen %}}

## Part One

Maybe the lists are only off by a small amount! To find out, pair up the numbers
and measure how far apart they are. Pair up the **smallest number in the left
list** with the **smallest number in the right list,** then the
**second-smallest left number** with the **second-smallest right number**, and
so on.

Within each pair, figure out **how far apart** the two numbers are; you'll need
to **add up all of those distances.** For example, if you pair up a 3 from the
left list with a 7 from the right list, the distance apart is 4; if you pair up
a 9 with a 3, the distance apart is 6.

To find the **total distance** between the left list and the right list, add up
the distances between all of the pairs you found. **What is the total distance
between your lists?**

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/01-historian-hysteria/HistorianHysteria.PartOne.cs"
  highlight="cs"
  id="HistorianHysteria.PartOne.cs">}}

Given the positional syntax in `readonly record struct LocationIds(List<int>
Left, List<int> Right)`, the compiler creates a `Deconstruct` method with an
`out` parameters for each positional parameter. However, `Deconstruct` ignores
properties that are defined using standard property syntax. {{% cite
RecordsPositionalSyntax %}}

## Part Two

This time, you'll need to figure out exactly how often each number from the left
list appears in the right list. Calculate a total **similarity score** by adding
up each number in the left list after multiplying it by the number of times that
number appears in the right list.

For example, the first number in the left list is 3. It appears in the right
list 3 times, so the similarity score increases by \\(3 \times 3 = 9\\). The
third number in the left list is 2. It does not appear in the right list once,
so the similarity score does not increase (\\(2 \times 0 = 0\\)). For the
example list, the similarity score at the end of this process is \\(9 + 4 + 0 +
0 + 9 + 9 = 31\\).

Once again, consider your left and right lists. **What is their similarity
score?**

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2024/AoC2024/01-historian-hysteria/HistorianHysteria.PartTwo.cs"
  highlight="cs"
  id="HistorianHysteria.PartTwo.cs">}}

## References

1. {{< citation
  id="StaticClasses"
  title="Static Classes and Static Class Members - C# | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/static-classes-and-static-class-members"
  accessed="2025-08-28" >}}

1. {{< citation
  id="RegexSourceGen"
  title=".NET regular expression source generators - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-source-generators"
  accessed="2025-08-28" >}}

1. {{< citation
  id="RecordsPositionalSyntax"
  title="Records - C# reference | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/record#positional-syntax-for-property-and-field-definition"
  accessed="2025-08-28" >}}

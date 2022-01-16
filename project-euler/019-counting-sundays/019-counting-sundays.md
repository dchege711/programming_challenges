---
authors:
- Edlund, Kristian
- Eisele, Robert
date: 2022-01-07
domains:
- docs.microsoft.com
- en.wikipedia.org
- projecteuler.net
- www.khanacademy.org
- www.mathblog.dk
- www.xarg.org
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/project-euler/019-counting-sundays/019-counting-sundays/
tags:
- cpp
title: 019. Counting Sundays
weight: 19
---

## Problem Statement {{% cite ProjectEuler019 %}}

You are given the following information, but you may prefer to do some
research for yourself:

* 1 Jan 1900 was a Monday.
* Thirty days has September,<br/> April, June and November.<br/> All the
  rest have thirty-one,<br/> Saving February alone,<br/> Which has
  twenty-eight, rain or shine.<br/> And on leap years, twenty-nine.
* A leap year occurs on any year evenly divisible by 4, but not on a
  century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?

## Solution

### Manual Attempt

{{< figure
  src="/img/computer-science/programming-challenges/project-euler/019-counting-sundays-manual.png"
  caption="[Failed manual attempt at solving 'Counting Sundays'](https://onedrive.live.com/view.aspx?resid=D3A50A924AE586F1%214828&id=documents&wd=target%28Project%20Euler.one%7C4AFF21B6-FEDE-2540-83FB-3D9780501572%2F019.%20Counting%20Sundays%7CBFAA8572-5B22-4243-AEE7-F117AA6DF265%2F%29)">}}

### Programmatic Attempt

{{< readfile file="content/computer-science/programming-challenges/project-euler/019-counting-sundays/counting_sundays.cc" highlight="cpp" >}}

Tried setting up a C++ build, like the one used in [the Chromium
Project](https://www.chromium.org/Home). Setting up
[Clang](https://clang.llvm.org/), [LLVM](https://llvm.org/),
[Ninja](https://ninja-build.org/manual.html) and
[GN](https://gn.googlesource.com/gn/+/refs/heads/main/docs/quick_start.md) took
some time. Mostly because I don't have a good understanding of the purpose of
each.

### Other People's Solutions

Some used the Date/Time libraries for their languages. For example, {{% cite
Edlund2013 %}} using C#:

```c#
int sundays = 0;

for (int year = 1901; year <= 2000; year++) {
    for (int month = 1; month <= 12; month++) {
        if ((new DateTime(year, month, 1)).DayOfWeek == DayOfWeek.Sunday) {
            sundays++;
        }
    }
}
```

{{% comment %}}

On the difference between .NET and C#, .NET is a dev platform for building
cross-platform apps, and C# is one of the languages in which one can use the
.NET platform. Other languages include F# and Visual Basic. {{% cite DotNetDocs
%}}

{{% /comment %}}

This didn't occur to me as it defeats the purpose of Project Euler. There's no
fun in letting the built-in functions do most of the work for you. That said,
it's pretty sweet how programming languages have made things easier for us.

{{% cite EiselePE019 %}} goes a different route. They mention Zeller's
Congruence, an algorithm for calculating the day of the week for any Julian or
Gregorian calendar date. For the Gregorian calendar, we have:

$$ h = \left( q + \lfloor \frac{13(m + 1)}{5} \rfloor + K + \lfloor K/4 \rfloor + \lfloor J/4 \rfloor - 2J \right) \text{mod } 7 $$

... where \\(h\\) is the day of the week (0 = Sat, 1 = Sun, ..., 6 = Fri),
\\(q\\) is the day of the month, \\(m\\) is the month (3 = Mar, 4 = Apr ..., 14
= Feb), \\(K\\) is the year of the century (\\(\text{year } \text{mod } 100\\)),
and \\(J\\) is the zero-based century (\\(\lfloor \text{year}/100 \rfloor\\)).
Jan and Feb are counted as months 13 and 14 of the previous year, e.g. Jan 1st,
1901 would be 13/01/1900 in MM/DD/YYYY format.

{{% cite WikiZellerCongruence %}}

{{% comment %}}

Maybe {{% cite WikiZellerCongruence %}} is what {{% cite ProjectEuler019 %}}
meant by "you may prefer to do some research for yourself"?

{{% /comment %}}

However, {{% cite WikiZellerCongruence %}} doesn't help us in solving the problem
faster. {{% cite EiselePE019 %}}'s final [annotated] solution is:

```js
function solution() {
  var numSundaysOnFirstDayOfMonth = 0;
  var dayOfWeekOfTheFirst = 2; // 01/01/1901 was a Tuesday
  var months = [31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

  for (var y = 1901; y <= 2000; y++) {
    // Update the numbers of days in February.
    months[1] = 28 + (y % 4 === 0 && y % 100 !== 0 || y % 400 === 0);

    for (var month of months) {
      // Adding 7 days results in the same day of the week. Adding the number of
      // days per month helps us go faster. We make use of the homomorphic rule:
      //
      //   (d + m) ≡ (d mod 7 + m mod 7) (mod 7)
      //
      dayOfWeekOfTheFirst += month % 7;

      if (dayOfWeekOfTheFirst % 7 === 0) {
        numSundaysOnFirstDayOfMonth++;
      }
    }
  }
  return numSundaysOnFirstDayOfMonth;
}
```

There are two things that I don't understand in {{% cite EiselePE019 %}}'s code.

First, why is `dayOfWeekOfTheFirst` given a value of `2` despite `2` being
Monday in Zeller's Congruence formula? If I change `dayOfWeekOfTheFirst` to to
`3` as per Zeller, and check Sundays by `dayOfWeekOfTheFirst % 7 === 1`, then
the code still gives the right answer. Maybe it's a matter of being consistent
with how you label the days of the week.

Second, which homomorphic property is \\( (d + m) \equiv (d \text{ mod } 7 + m
\text{ mod } 7) \ (\text{mod } 7) \\)? Going by {{% cite
KhanAcademyModularAdditionSubtraction %}}, that's a known result in modular
arithmetic.

{{% comment %}}

See [Modular Arithmetic]({{< ref
"/mathematics/cryptography/modular-arithmetic.md" >}}) for a review of modular
arithmetic.

{{% /comment %}}

## References

1. {{< citation
  id="ProjectEuler019"
  title="#19 Counting Sundays - Project Euler"
  url="https://projecteuler.net/problem=19"
  accessed="2022-01-07">}}

1. {{< citation
  id="Edlund2013"
  authors="Kristian Edlund"
  title="Project Euler 19: How many Sundays on the first of a month in C# | MathBlog"
  url="https://www.mathblog.dk/project-euler-19/"
  year="2013"
  accessed="2022-01-08" >}}

1. {{< citation
  id="DotNetDocs"
  title=".NET documentation | Microsoft Docs"
  url="https://docs.microsoft.com/en-us/dotnet/"
  url_2="https://docs.microsoft.com/en-us/dotnet/core/introduction"
  accessed="2022-01-08">}}

1. {{< citation
  id="EiselePE019"
  author="Robert Eisele"
  title="Project Euler 19 Solution: Counting Sundays • Computer Science and Machine Learning"
  url="https://www.xarg.org/puzzle/project-euler/problem-19/"
  accessed="2022-01-08">}}

1. {{< citation
  id="WikiZellerCongruence"
  title="Zeller's congruence - Wikipedia"
  url="https://en.wikipedia.org/wiki/Zeller's_congruence"
  accessed="2022-01-08">}}

1. {{< citation
  id="KhanAcademyModularAdditionSubtraction"
  title="Modular addition and subtraction (article) | Khan Academy"
  url="https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/modular-addition-and-subtraction"
  accessed="2022-01-10">}}

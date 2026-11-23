---
date: 2026-11-19
domains:
- github.com
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/low-level-design/dasher-payout/dasher-payout/
title: Dasher Payout Calculation
---

## Problem Statement

Design an API that calculates total payout for a driver based on their delivery
events. Each delivery event has a status that's one of `accepted`, `delivered`,
or `cancelled`. The payout for completed deliveries is:

```py
payout = base_rate * (1 + bonus_factor * concurrent_count)
```

... where `concurrent_count` is determined at the time of accepting the delivery
request. {{% cite DasherPayoutHelloInterview %}}

## Solution

### Parse, Don't Validate

{{% cite DasherPayoutHelloInterview %}} passes delivery events of the form:

```js
{id: "D1", status: "accepted", time: "10:00"}
```

... and so parse them into appropriate types:

{{< detail-readFile
  file="/content/computer-science/programming-challenges/low-level-design/dasher-payout/DasherPayout/DeliveryEvent.cs"
  highlight="cs" >}}

{{< detail-readFile
  file="/content/computer-science/programming-challenges/low-level-design/dasher-payout/DasherPayout/Converters.cs"
  highlight="cs" >}}

... where `enum DeliveryStatus` affords us stricter typing on the possible
statuses, and `TimeOnly Time` gets us a sorting key, e.g., "8:00" is less than
"10:00".

{{% comment %}}

The canonical input is to have a timestamp based on a pre-agreed epoch. Sending
timestamps as strings is a weird choice on {{% cite DasherPayoutHelloInterview
%}}'s part.

{{% /comment %}}

Using a `double` for money introduces rounding errors that are not appropriate
for financial calculations, which require large numbers and no round-off errors.
.NET's `Decimal` fills this gap, supporting values in the range \\([-(2^{96} -
1), 2^{96} - 1]\\):

```cs
Console.WriteLine(0.1  + 0.2  == 0.3);  // False: double
Console.WriteLine(0.1m + 0.2m == 0.3m); // True: decimal
```

{{% cite DotNetDecimal %}}

{{% open-comment %}}

Find practice problems that demand an understanding of rounding off errors to
further cement this.

{{% /open-comment %}}

### Payout Calculation

I initially "reduced" this problem to a harder problem where I parsed the
delivery events into a sequence of intervals:

```cs
private record struct Delivery(
  string Id, TimeOnly Start, TimeOnly End, DeliveryTerminalStatus Status);
```

... and then proceeded to sweep through the intervals in order, and extract
concurrent counts.

{{< figure
  src="/img/computer-science/low-level-design/dasher-payout/interval-sweep.svg"
  caption=`Sweeping through intervals. Credits: GPT-6.1 Sol.` >}}

However, the sweep ends up re-creating the original events, e.g.,
`Delivery.Start` is the original `Delivery.Accepted` event! Processing the
`DeliveryEvent`s in increasing `Time` order gets us there:

{{< detail-readFile
  file="/content/computer-science/programming-challenges/low-level-design/dasher-payout/DasherPayout/DasherPayout.cs"
  highlight="cs" >}}

{{% comment %}}

The failure mode here was having just enough information to trip myself up. I
vaguely remembered some computational geometry algorithms from college, and
rushed to assume that this was an instance of it.

The algorithm involved two instances of the `Delivery` lists, one sorted by
`Delivery.Start` and the other sorted by `Delivery.End`. We then advanced a
pointer in each list, while computing the overlapping intervals along the way.
{{% cite DasherPayoutAtb0a598c %}}

{{% /comment %}}

## References

1. {{< citation
  id="DasherPayoutHelloInterview"
  title="Design Dasher Payout Calculation System Based on Delivery Events | Hello Interview"
  url="https://www.hellointerview.com/community/questions/dasher-payout-classes/cmdckvv0i008qad08os5l6gv2"
  accessed="2026-11-19" >}}

1. {{< citation
  id="DasherPayoutAtb0a598c"
  title="blog/content/computer-science/low-level-design/exercises/dasher-payout/DasherPayout/DasherPayout.cs at b0a598c99f9611ca66fad95b53e6b2c25217caba · dchege711/blog"
  url="https://github.com/dchege711/blog/blob/b0a598c99f9611ca66fad95b53e6b2c25217caba/content/computer-science/low-level-design/exercises/dasher-payout/DasherPayout/DasherPayout.cs"
  accessed="2026-11-22" >}}

1. {{< citation
  id="DotNetDecimal"
  title="Decimal Struct (System) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.decimal?view=net-10.0"
  accessed="2026-11-22" >}}

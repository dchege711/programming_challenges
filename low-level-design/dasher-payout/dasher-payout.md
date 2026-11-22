---
date: 2026-11-19
domains:
- www.hellointerview.com
draft: true
local_url: http://localhost:1313/computer-science/programming-challenges/low-level-design/dasher-payout/dasher-payout/
title: 'API for Dasher Payout Calculation System'
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

## My Solution

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/dasher-payout/DasherPayout/External/DeliveryEvent.cs"
  highlight="cs" >}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/dasher-payout/DasherPayout/Converters.cs"
  highlight="cs" >}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/dasher-payout/DasherPayout/DeliveryEvent.cs"
  highlight="cs" >}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/dasher-payout/DasherPayout/DasherPayout.cs"
  highlight="cs" >}}

## References

1. {{< citation
  id="DasherPayoutHelloInterview"
  title="Design Dasher Payout Calculation System Based on Delivery Events | Hello Interview"
  url="https://www.hellointerview.com/community/questions/dasher-payout-classes/cmdckvv0i008qad08os5l6gv2"
  accessed="2026-11-19" >}}

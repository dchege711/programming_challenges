---
title: Self-hosting
date: 2024-03-27
---

## Current State: SaaS

Currently, we're on [Render's free tier](https://docs.render.com/free)
that gives us these free web services:

* Custom domains
* Managed TLS certificates
* Pull request reviews
* Log streams
* Rollbacks up to the two most recent previous deploys.

... with these limitations:

* Spins down after 15min of no inbound traffic. Spinning up on the next request causes a noticeable delay for a couple seconds.
* Monthly limits, which will suspend the app if exceeded:
  * 750 instance hours.
  * 100GB bandwidth.
  * 500 pipeline minutes.
* Can be restarted at any time by Render.

From [this SO post](https://stackoverflow.com/a/75680364), we created a
cron job at [console.cron-job.org](https://console.cron-job.org/) that
pings [https://cards.c13u.com/wiki](https://cards.c13u.com/wiki) every
14min. \\(31 \times 25 = 775\\) which means the app will be down on the
31st of every month.

Render's free PostgreSQL database becomes inaccessible 90 days after
creation. For our DB needs, we're using [MongoDB's free
tier](https://www.mongodb.com/pricing), which has shared RAM, and 512MB
to 5GB of storage.

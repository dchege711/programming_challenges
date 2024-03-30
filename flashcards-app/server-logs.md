---
title: Server Logs
date: 2024-03-27
---

The app is [hosted at
render.com](https://dashboard.render.com/web/srv-cdb17nqen0hldb3lqj6g),
and Render suggests [several options for
logging](https://docs.render.com/log-streams).

[Better Stack](https://betterstack.com/docs/logs/render/)'s [free
tier](https://betterstack.com/logs/pricing) offers 1GB/month, 3-day
retention and 5 data sources. It also has an Uptime product whose [free
tier](https://betterstack.com/uptime/pricing) has free email alerts,
3-minute checks, 10 monitors, and 5 status pages.

[Datadog](https://docs.render.com/datadog#setting-up-log-streams)'s
[pricing
page](https://www.datadoghq.com/pricing/?product=log-management#products)
starts at $0.10/GB. It also has logging integrations for
[network](https://docs.datadoghq.com/integrations/network/), [disk usage
and IO](https://docs.datadoghq.com/integrations/disk/) and
[Postgres](https://docs.datadoghq.com/integrations/postgres/?tab=host).
The pricing for the Postgres monitoring isn't clear: [this
page](https://www.datadoghq.com/pricing/?product=database-monitoring#products)
says $70 per database host, per month, but [the Postgres integration
page](https://docs.datadoghq.com/integrations/postgres/?tab=host) states
that the standard Postgres Agent integration is different. [This Reddit
thread](https://www.reddit.com/r/devops/comments/zz4naq/datadog_i_do_not_understand_the_pricing_model/)
jokes that their pricing model involves signing over your company and
all its assets.

[highlight.io](https://www.highlight.io/docs/getting-started/backend-logging/hosting/render)'s
[free tier](https://www.highlight.io/pricing) has 500 sessions, 1k
errors, 1m logs and 25m traces per month.

[Papertrail](https://www.papertrail.com/plans/)'s cheapest plan is
$7/month.

[Sumo
Logic](https://help.sumologic.com/docs/send-data/hosted-collectors/cloud-syslog-source/#configure-a-cloudsyslogsource)'s
[free
tier](https://www.sumologic.com/pricing/#get-what-you-need-for-monitoring-troubleshooting-and-security)
has daily limits of 1GB logs, 3k DPM metrics, 1.5GB of traces.

Datadog is out for being vague with pricing and rumored to be expensive.
Papertrail costs money, and the flashcard app does not make money. Sumo
Logic seems like the clear winner with their generous free tier, [or
maybe
not](https://www.reddit.com/r/devops/comments/12jhxbg/grafana_to_sumologic_pricing/)?

[The Complete Guide to the ELK
Stack](https://logz.io/learn/complete-guide-elk-stack/#what-elk-stack)
summarizes the state of self-hosted open-source logging solutions. The
ELK stack combines Elasticsearch ( a full-text search and analysis
engine), Logstash (a log aggregator), and Kibana (a visualization
layer). In 2021, Elastic made the ELK stack no longer open source,
prompting AWS's OpenSearch and OpenSearch Dashboards.

The main motivation for collecting logs today is to find out when users
tend to log in, and thus reduce the cold start.

---
date: 2024-04-18
domains:
- open-wc.org
- webcomponents.dev
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/web-components/
title: Using Web Components
weight: 1
---

What does it take to use web components? {{% cite openWCBaseLibraries
%}} lists several options, and
[Google's](https://lit.dev/docs/#why-should-i-choose-lit)
[`lit`](https://lit.dev/) is leading the pack in popularity at 1.6M
weekly downloads. [`Stencil`](https://stenciljs.com/) (703K,
[Ionic](https://stenciljs.com/docs/faq#why-was-stencil-created)),
[`solid-js`](https://www.solidjs.com/) (269K,
[Netlify](https://dev.to/ryansolid/when-netlify-asks-you-to-full-time-oss-you-say-yes-5ccf)),
and `FAST` (88K, [Microsoft](https://www.fast.design/)) round up the
list of libraries with +50K weekly NPM downloads.

{{% cite allTheWaysDivRiot %}} collects stats on bundle sizes and
performance. Notably, `React` falls in the lower end with large bundle
sizes and poor performance. Granted, `React` is still pretty popular
with [22M weekly NPM downloads](https://www.npmjs.com/package/react).

1. {{< citation
  id="openWCBaseLibraries"
  title="Community: Base Libraries: Open Web Components"
  url="https://open-wc.org/guides/community/base-libraries/"
  accessed="2024-04-23" >}}

1. {{< citation
  id="allTheWaysDivRiot"
  title="All the Ways to Make a Web Component - Feb 2022 Update"
  url="https://webcomponents.dev/blog/all-the-ways-to-make-a-web-component/"
  accessed="2024-04-23" >}}

---
cited-authors:
- Carter, Quin
date: 2024-04-20
domains:
- github.com
- lit.dev
- medium.com
- www.fast.design
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/web-components/browse-page/
title: Browse Page
weight: 3
---

{{< figure
  src="/img/computer-science/programming-challenges/flashcards/web-components/browse-landing-page.jpg"
  caption=`When a user lands at /browse, this UI is shown. A couple of
  components seem to emerge: search-bar, tags-list, card-results, and
  mini-card.` >}}

## `<search-bar>`

Currently, this is rendered by `search_bar_dropdown.ejs`, a partial that
that is included in both `/home` and `/browse`. The fact that there it
has JS, no server-delivered content, and has CSS makes it a good
candidate for a web component. Revving up `search-bar.ts`.

{{% comment %}}

Ran into `Uncaught TypeError: Class constructor s cannot be invoked
without 'new'` on the `export class SearchBar extends LitElement` line.
This happens because my `tsconfig.json` targets `ES5` which didn't have
`class`es. {{% cite es6Features %}} Hopefully [the `ES6` detour won't
cost a lot]({{< ref "../of-builds-and-bundlers.md" >}}).

{{% /comment %}}

Pleasantly surprised at how encapsulated
[`<search-bar>`](https://github.com/dchege711/study_buddy/blob/8f2c3d8f32ce47ca81758ff2d8c2c4e142bf70a5/src/public/src/components/search-bar/search-bar.ts)
is. The core design is:

```html
<search-bar>
  <input />
  <ul></ul>
</search-bar>
```

... where the `<ul>` shows the search results, and on pressing `Enter`,
`<search-bar>` dispatches a `CardSearchResultsEvent` containing the
search results. {{% cite eventsLit %}} talks about an "events go up;
properties come down" UI approach that works well in `<search-bar>`'s
case. Down the line, we want the `<search-results>` element to
display the results.

## `<search-results>`

Currently, the structure is basically:

```html
<div id='temp_container'>
  <div>
    <div id='minicards_search_results'>
      <div class='minicard_search_result'></div>
      ...
      <div class='minicard_search_result'></div>
    </div>
    <div id="card_modal">...</div>
  </div>
</div>
```

... where `#temp_container` is `<search-bar>`'s sibling.

Given that `<search-results>` is not `<search-bar>`'s
ancestor, the `CardSearchResultsEvent` will not bubble to it. Is event
dispatch the correct design? For that event to be reflected in
`<search-results>`, some common ancestor needs to handle it,
and send down `CardSearchResultsEvent.results`.

Lit also has a `Context` protocol for providing contextually available
data such that ancestor elements in between the provider and the
consumer aren't even aware of the data. Use cases usually include an
app's data store, the current user, a UI theme, etc. `Context` is built
on top of DOM events. {{% cite contextLit %}} `Context` is still based
on a DOM events, so we'll still need a common ancestor between
`<search-results>` and `<search-bar>`, maybe call that `<browse-page>`.

{{% comment %}}

FAST Web Components offer a central place for defining a prefix for the
web components' names, e.g., `cd-search-results`, `cd-search-bar`, and
`cd-browse-page`. {{% cite fastDesignSystem %}} Lit doesn't appear to
have an equivalent.

{{% /comment %}}

{{% cite contextLit %}} did not click until after reading {{% cite
Carter2023 %}}. The context provider is usually some top-level node.
Lower-level nodes do not directly set the context data; instead, they
dispatch events up the tree, where the root component catches them and
updates the context. Context consumers are usually grandchildren or
deeper in the hierarchy. There is no need for immediate children to be
context consumers given that the context provider can provide bindings
in the HTML template.

## References

1. {{< citation
  id="es6Features"
  title="lukehoban/es6features: Overview of ECMAScript 6 features"
  url="https://github.com/lukehoban/es6features?tab=readme-ov-file#classes"
  accessed="2024-04-20" >}}

1. {{< citation
  id="eventsLit"
  title="Events – Lit"
  url="https://lit.dev/docs/components/events/"
  accessed="2024-04-21" >}}

1. {{< citation
  id="contextLit"
  title="Context – Lit"
  url="https://lit.dev/docs/data/context/"
  accessed="2024-04-21" >}}

1. {{< citation
  id="fastDesignSystem"
  title="The FAST Frame Design System | FAST"
  url="https://www.fast.design/docs/design-systems/fast-frame/#designsystemwithprefix"
  accessed="2024-04-23" >}}

1. {{< citation
  id="Carter2023"
  author="Quin Carter"
  title="Understanding Component State and Using Lit Element Context with Web Components | by Quin Carter | Medium"
  url="https://medium.com/@quincarter/understanding-component-state-and-using-lit-element-context-40981e808535"
  date="2023-02-26"
  accessed="2024-04-25" >}}

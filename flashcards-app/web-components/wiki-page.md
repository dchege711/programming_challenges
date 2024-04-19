---
title: Wiki Page
date: 2024-04-18
weight: 1
---

## Overview

Starting with this page because it's mostly static. The body is
basically:

```html
<div class="topnav">...</div>
<div id="main_div">
  <div id="study_buddy_details">
    <div class="details_section">...</div>
    <div class="details_section">...</div>
    ...
    <div class="details_section">...</div>
  </div>
</div>
<footer>...</footer>
```

The corresponding web components would be something along the lines of:

```html
<nav-bar></nav-bar>
<wiki-container>
  {Table of Contents}
  <wiki-section></wiki-section>
  <wiki-section></wiki-section>
</wiki-container>
```

## `<wiki-container>`

Maybe `<wiki-section>` can be a template owned by `<wiki-container>`?
Nah, there are benefits to a separate `<wiki-section>` component, e.g.,
each component can define its title for use in the ToC. That way, new
`<wiki-section>`s can be added with minimal changes to
`<wiki-container>`.

Inheritance doesn't work well. Given:

```ts
@customElement('wiki-section')
export class WikiSection extends LitElement {
  get title() : string {
    return '';
  }

  render() {
    return html``;
  }
}
```

... how do I create a `MigrationWikiSection` that still renders a
`wiki-section`, but overrides `title` and `render`? It is feasible
though to define an interface:

```ts
interface WikiSection {
  title: string;
  content: TemplateResult;
}
```

... have individual wiki sections of the form:

```ts
export const MigrationWikiSection: WikiSection = {
  title: 'Migration to cards.curiosities.dev';
  content = html`...`;
}
```

... and now I don't need an explicit `<wiki-section>` component given
that its only use is rendering `WikiSection.content`.

Do I even need the `<wiki-container>` component? So far, everything in
it is known at build time, and so server-side rendering makes sense. No
need to populate the `WikiSection`s via JavaScript.

## `<nav-bar>`

In a similar vein, what does a `<nav-bar>` component bring? The current
navigation bar is a partial EJS template, `navbar.ejs`.

The server already knows if the user is logged in, and decides whether
to show the `Log Out` and `Your Account` buttons.

Encapsulating the style is somewhat desirable, but I think we can
achieve that with [`<template
shadowrootmode="open">...</template>](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template#implementing_a_declarative_shadow_dom).

There is a script that issues a `POST` request to `/logout`, and on
success (the return value doesn't matter) clears `localStorage` and sets
`window.location.href` to `/`. The last bit can be done server-side by
issuing a redirect to `/`. [Why do we need `localStorage` though?]({{<
ref "../use-of-local-storage" >}}).

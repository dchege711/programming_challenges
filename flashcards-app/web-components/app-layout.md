---
date: 2024-04-19
domains:
- ejs.co
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/web-components/app-layout/
title: App Layout
weight: 2
---

Is there a use-case for the container that lays out the various app
elements? [When considering a `<nav-bar>`]({{< ref
"./wiki-page#nav-bar" >}}), the conclusion was that a web component
wasn't necessary.

## Possible Organization

The high-level picture is:

```html
<body>
  <nav>
    <div id="topnav-banner">...</div>
    <ul id="topnav-items">...</ul>
  </nav>
  <div id="main_div">...</div>
  <footer>...</footer>
</body>
```

We should move `#topnav-banner` out of the `<nav>` so that we can
potentially apply lateral spacing to `#topnav-items` and `#main_div`
without it applying to `#topnav-banner`.

## Server-Rendered EJS Does Not Support Blocks

The app currently uses EJS templates on the server. EJS does not
specifically support blocks, but layouts can be implemented through
`include`s, e.g.,

```js
<%- include('header'); -%>
<h1>Title</h1>
<p>My page</p>
<%- include('footer'); -%>
```

I'll need to implement the layout in the `<body>`'s CSS. At this point,
a ShadowDOM doesn't offer much use because there's not anything that we
need to isolate the `<body>` from.

## References

1. {{< citation
  id="ejsDocs"
  title="EJS -- Embedded JavaScript templates"
  url="https://ejs.co/#docs"
  accessed="2024-04-19" >}}

---
title: Of Stale UI and Re-renders
date: 2024-05-28
---

More than once, I've been surprised by a web component either showing
data that should no longer be there, or not showing data that should be
there. This page aims to reason through such cases for a better mental
model of web components.

## Rendering Lists

Both `<search-bar>` and `<search-results>` need to render a collection
of N items. {{% cite listsLit %}} offers two options: looping, or using
the `repeat(items, keyFunction, itemTemplate)` directive.

```ts
const cards = html`
  ${cards.map(
    (card) => html`<div><input type="checkbox">${card.title}</div>`
  )}
`;

const template2 = html`
  ${repeat(
    cards, (card) => card.id
    (card) => html`<div><input type="checkbox">${card.id}</div>`
  )}
`;
```

When performing updates, `repeat` moves DOM nodes, while `map` reuses
DOM nodes. This is also beneficial when there is some part of the node
that isn't controlled by a template expression because `repeat` will
keep that state, e.g, in the example above, the `checked` property. If
none of these apply, then `map` or loops can be used over `repeat`. {{%
cite listsLit %}}

{{% comment %}}

While React does not have a dedicated `repeat` equivalent, React issues
a `Warning: Each child in a list should have a unique "key" prop` when
rendering a list without specifying keys for the individual items. {{%
cite listsReact %}}

The recommended behaviors in Lit and React make it seem like using keys
is almost always the correct approach to use. We'd need perf numbers to
justify not using keys.

{{% /comment %}}

## Re-renders on Tagged Templates

Given a component like:

```ts
@customElement('sample-app')
export class SampleApp extends LitElement {
  @state() private content = 'Hello world';
  @state() private counter = 0;

  constructor() {
    super();
    setInterval(() => this.counter++, 1000);
  }

  render() {
    return html`<p>${this.content}</p>`;
  }
}
```

`SampleApp.render` will be called every second {{% cite litUpdatingState
%}}. The fact that `this.counter` is not referenced in the rendered
template does not prevent an update cycle. In this case, `this.counter`
need not be reactive; a vanilla instance variable will do.

In this example though:

```ts
function getTimeString() {
  let now = new Date(Date.now());
  return `${now.getMinutes()}:${now.getSeconds()}`;
}

@customElement('input-wrapper')
class InputWrapperElement extends LitElement {
  render() {
    return html`<input value=${getTimeString()} >`;
  }
}

@customElement('sample-app')
export class SampleApp extends LitElement {
  @state() content = 'Hello world';

  constructor() {
    super();
    setInterval(this.updateContent.bind(this), 1000);
  }

  render() {
    return html`
      <div>Expected value: ~${this.content}</div>
      <input-wrapper></input-wrapper>
    `;
  }

  private updateContent() {
    this.content = getTimeString();
  }
}
```

`<input>` does not update every second {{% cite litWrappedInput %}}.
This might be because `<sample-app>` decides that `<input-wrapper>`
should not be updated.

What if we want to reset `<input>` from `SampleApp.updateContent()`?
Exposing `InputWrapperElement.reset()` could do the trick, but that
breaks away from the "events go up; properties come down" philosophy for
encapsulated web components. Syncing `InputWrapperElement.value` with
the `<input>`'s value does not work either. {{% cite
litWrappedInputSynced %}}

## References

1. {{< citation
  id="listsLit"
  title="Lists – Lit"
  url="https://lit.dev/docs/templates/lists/"
  accessed="2024-05-28" >}}

1. {{< citation
  id="listsReact"
  title="Rendering Lists – React"
  url="https://react.dev/learn/rendering-lists"
  accessed="2024-05-28" >}}

1. {{< citation
  id="litUpdatingState"
  title="Lit Playground - Updating State"
  accessed="2024-06-02"
  url="https://lit.dev/playground/#sample=v3-docs%2Fcomponents%2Foverview%2Fsimple-greeting&gist=bec4f87f52af81e81d53bcd90ee5be79" >}}

1. {{< citation
  id="litWrappedInput"
  title="Lit Playground - Wrapped Input Element"
  accessed="2024-06-02"
  url="https://lit.dev/playground/#sample=v3-docs%2Fcomponents%2Foverview%2Fsimple-greeting&gist=db11191cca7ca3d5aaa0b9f6285bfa55" >}}

1. {{< citation
  id="litWrappedInputSynced"
  title="Lit Playground - Wrapped Input Synced Props"
  accessed="2024-06-02"
  url="https://lit.dev/playground/#sample=v3-docs%2Fcomponents%2Foverview%2Fsimple-greeting&gist=e32781380feff4768ad2d97c72d661be" >}}

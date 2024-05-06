---
title: Reusable Cards
date: 2024-05-03
weight: 5
---

## Context

How can I reuse code in these user experiences?

{{< figure
  src="/img/computer-science/programming-challenges/flashcards/web-components/card-types.jpg"
  caption=`Current card templates. TL: editable card owned by the user.
  TR: the overall modal experience. BL: a publicly viewable card. BR: a
  card owned by the user but is in the trash.` >}}

The current API surface for the different types of UIs is:

```ts
interface EditableCardViewer {
  displayNewCard(): void;
  displayFullCard(cardId: string): void;
  renderCard(card: Partial<ICard> | null): void;

  fetchCard(caller: () => Promise<Partial<ICard> | null>): void;
  fetchPreviousCard(): void;
  fetchNextCard(): void;

  updateStreakBar(streak: IStreak): void;

  handleInputChange(elementId: string): void;
  handleCardUrgencyChange(): void;
  handleTagsInputChange(ev: KeyboardEvent): void;

  displayRawCardDescription(): void;
  insertTabsIfNecessary(ev: KeyboardEvent): void;

  saveCard(renderAfterSave: Boolean = true): void;
  moveCardToTrash(): void;
  restoreCardFromTrash(cardId: string, cardUrgency: number): void;

  toggleOption(elementId: "reviewModeToggle" | "card_is_public_toggle"): void;
  makeInvisible(elementId: string): void;
  colorUrgencyQuartiles(quartiles: number[]): void;

  suggestNewTags(tagsInputElement: HTMLInputElement): void;
  removeTagSuggestions(): void;
  updateTagsButtons(newTag: string): void;
  removeTagFromCard(tag: string): void;
}

interface PublicCardViewer {
  displayFullCard(cardId: string): void;
  renderCard(card: Partial<ICard> | null): void;

  fetchPreviousCard(): void;
  fetchNextCard(): void;

  handleSearchInputChange(): void;

  flagCard(reason: "markedForReview" | "markedAsDuplicate"): void;
  copyCardToOwnCollection();
}

interface TrashedCardViewer {
  renderTrashedCard(card: Partial<ICard> | null);

  fetchPreviousCard(): void;
  fetchNextCard(): void;

  modifyTrash(endpoint: "/delete-card" | "/restore-from-trash"): void;
}
```

The web components v0 has:

```ts
class CardViewer extends LitElement {
  @property({type: Object})
  protected card: Card | null = null;

  // If card changed and is non-null, show the modal.
  updated(changedProperties: Map<string, any>): void {...}

  protected updateCarouselCursor(direction: CardsCarouselUpdateCursorDirection): void {...}

  protected closeDialog(): void {...}

  // Throws error as it is supposed to be overriden.
  protected renderCard(): TemplateResult {...}

  // Calls `renderCard` and wraps the result in a <dialog> element.
  render(): TemplateResult {...}

  // Style dialog, backdrop, and code blocks.
  static styles = [ ... ]
}

@customElement('public-card-viewer')
class PublicCardViewer extends CardViewer {
  @property({ type: Object})
  protected card: PublicCardResult = null;

  private flagCard(reason: FlagReason): void {...}

  // Can use `CardViewer.prototype.*` properties.
  protected renderCard(): TemplateResult {...}

  render() { return super.render(); }

  static styles = [...super.styles, css`...` ]
}
```

... and then `<browse-page>` has:

```html
<public-card-viewer .card=${this.selectedResult}>
</public-card-viewer>
```

## Composition

{{% cite CompositionLit %}}'s approach is essentially:

```html
<editable-card>
  <card-top-bar></card-top-bar>
  <card-title-bar></card-title-bar>
</editable-card>
```

Properties go down and events go up. To pass data between siblings, the
common ancestor would act as a mediator, e.g., `<editable-card>` listens
for events from `<card-top-bar>` and then sets properties
`<card-title-bar>`. {{% cite CompositionLit %}}

Composition can also use slotted children, e.g.,

```html
<base-card>
  <slot name='top-bar'><slot>
  <slot name='title-bar'><slot>
</base-card>

<editable-card>
  <base-card>
    <div slot='top-bar'>...</div>
    <div slot='title-bar'>...</div>
  </base-card>
</editable-card>
```

... with the caveat that `<base-card>` cannot make any assumptions about
the slotted content because that content is controlled by and can be
added/removed at any time. {{% cite slottedChildrenLit %}}

## Reactive Controllers

This would take the form of:

```ts
export class MyController implements ReactiveController {
  host: ReactiveControllerHost;

  constructor(host: ReactiveControllerHost) {
    (this.host = host).addController(this);
  }

  // Do something and then call `this.host.requestUpdate()`.
  hostConnected(): void {...}
}

@customElement('my-element')
class MyElement extends LitElement {
  private controller = new MyController(this);
  render() {
    return html`Current value: ${this.controller.value}`;
  }
}
```

... where a `ReactiveController` can hook into lifecycle methods such as
`hostConnected`, `hostUpdate`, `hostUpdated`, and `hostDisconnected`.
{{% cite reactiveControllers %}}

## Mixins

Unlike reactive controllers which implement composition patterns, mixins
offer abstract subclasses that can be combined into concrete classes.
{{% cite mixinsLit %}}

{{% comment %}}

[More on mixins and inheritance in general]({{< ref
"/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/inheritance"
>}}).

{{% /comment %}}

This would be something of the form:

```ts
declare class CardViewerMixinInterface {
  @property({type: Object}) protected card: Card | null;
  updated(changedProperties: Map<string, any>): void;
  protected updateCarouselCursor(direction: CardsCarouselUpdateCursorDirection): void;
  protected closeDialog(): void;
  protected renderCard(): TemplateResult;
  render(): TemplateResult;
  static styles: CSSResult[];
}

const CardViewerMixin = <T extends LitElementConstructor>(superClass: T) => {
  class CardViewerMixinClass extends superClass implements CardViewerMixinInterface {...};
  return CardViewerMixinClass as Constructor<CardViewerMixinInterface> & T;
}

declare class PublicCardViewerMixinInterface {
  @property({type: Object}) protected card: PublicCardResult;
  protected renderCard(): TemplateResult;
  static styles: CSSResult[];
}

const PublicCardViewerMixin = <T extends LitElementConstructor>(superClass: T) => {
  class PublicCardViewerMixinClass extends superClass implements PublicCardViewerMixinInterface {...};
  return PublicCardViewerMixinClass as Constructor<PublicCardViewerMixinInterface> & T;
}

@customElement('public-card-viewer')
class PublicCardViewer extends PublicCardViewerMixin(CardViewerMixin(LitElement)) {};
```

This seems overly complicated. `PublicCardViewer` needs to be applied to
a `CardViewer` instance and this superclass is known at class definition
time; the mixin adds more code unnecessarily. What about
`CardViewerMixin`? Its superclass, `LitElement`, is also known at class
definition. Mixins seem overly complicated for my use case. Plain
inheritance it is unless there's a reason to do otherwise.

## References

1. {{< citation
  id="CompositionLit"
  title="Component composition – Lit"
  url="https://lit.dev/docs/composition/component-composition/"
  accessed="2024-05-03" >}}

1. {{< citation
  id="slottedChildrenLit"
  title="Working with Shadow DOM – Lit"
  url="https://lit.dev/docs/components/shadow-dom/#slots"
  accessed="2024-05-03" >}}

1. {{< citation
  id="reactiveControllers"
  title="Reactive Controllers – Lit"
  url="https://lit.dev/docs/composition/controllers/"
  accessed="2024-05-03" >}}

1. {{< citation
  id="mixinsLit"
  title="Mixins – Lit"
  url="https://lit.dev/docs/composition/mixins/"
  accessed="2024-05-03" >}}

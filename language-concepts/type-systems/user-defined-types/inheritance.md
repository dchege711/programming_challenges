---
cited-authors:
- Fagnani, Justin
date: 2024-05-05
domains:
- developer.mozilla.org
- justinfagnani.com
- lit.dev
- stackoverflow.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/inheritance/
title: Inheritance
---

## Mixins

{{% comment %}}

Introduced to mixins by {{% cite mixinsLit %}}. Didn't know that there
are passionate advocates for this, e.g., {{% cite fagnani2015 %}}'s "You
can even look at normal subclass inheritance as a degenerate form of
mixin inheritance where the superclass is known at class definition
time, and there's only one application of it."

{{% /comment %}}

A mix-in is an abstract subclass. This technique is especially useful in
languages where a class can only have a single superclass.

```js
const calculatorMixin = (Base) =>
  class extends Base {
    calc() {}
  };

const randomizerMixin = (Base) =>
  class extends Base {
    randomize() {}
  };

class Foo {}
class Bar extends calculatorMixin(randomizerMixin(Foo)) {}
```

{{% cite mixinsMDN %}}

Folks use mixins to provide a lot of optional features for a class, or
use one particular feature in a lot of different classes. One way to
think about mixins is a small base type designed to add a small amount
of functionality to a type without otherwise affecting that type. {{%
cite mixinUsefulnessSO %}} Mixins are rarely useful as standalone
objects. One can think of it as an interface with an associated
implementation. {{% cite mixinVsInheritanceSO %}}

1. {{< citation
  id="mixinsLit"
  title="Mixins – Lit"
  url="https://lit.dev/docs/composition/mixins/"
  accessed="2024-05-03" >}}

1. {{< citation
  id="mixinsMDN"
  title="extends > Examples > Mix-ins"
  url="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/extends#mix-ins"
  accessed="2024-05-05" >}}

1. {{< citation
  id="fagnani2015"
  author="Justin Fagnani"
  title="'Real' Mixins with JavaScript Classes"
  url="https://justinfagnani.com/2015/12/21/real-mixins-with-javascript-classes/"
  date="2015-12-21"
  accessed="2024-05-05" >}}

1. {{< citation
  id="mixinUsefulnessSO"
  title="python - What is a mixin and why is it useful? - Stack Overflow"
  url="https://stackoverflow.com/questions/533631/what-is-a-mixin-and-why-is-it-useful"
  accessed="2024-05-05" >}}

1. {{< citation
  id="mixinVsInheritanceSO"
  title="OOP - Mixin vs inheritance - Stack Overflow"
  url="https://stackoverflow.com/questions/860245/mixin-vs-inheritance"
  accessed="2024-05-05" >}}

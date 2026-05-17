---
cited-authors:
- Wilson, Greg
date: 2026-05-16
domains:
- en.wikipedia.org
- third-bit.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/type-systems/user-defined-types/dict-based-impl-of-objects-and-class-python/
title: Dictionary-Based Implementation of Classes and Objects (Python)
---

{{% cite SDXPyObjectsAndClasses %}} implements a toy version of Python's object
system using dictionaries that contain references to properties, functions and
other dictionaries. Consider two shapes, `Square` and `Circle`, with the methods
`perimeter`, `area`, and `density`.

## Objects

A function is an object, where the bytes in a function are instructions. For
example:

```py
def foo():
  print("in foo")
```

... creates an object in memory that contains instructions to print a string,
and assigns that object to the variable `foo`. {{% cite SDXPyObjectsAndClasses
%}}

{{% comment %}}

Example of moving up a rung in the abstraction ladder. Bytes can be interpreted
as text, images, instructions, and more.

{{% /comment %}}

Consequently, we can define squares as:

```py
def square_perimeter(sq):
    return sq["side"] * 4

def square_area(sq):
    return sq["side"] ** 2

def square_new(side):
    return {
        "name": "square",
        "side": side,
        "perimeter": square_perimeter, # A function can be assigned like any other type
        "area": square_area,
    }

def call(thing, method_name):
    return thing[method_name](thing)

def test():
    square = square_new(3)

    assert square["name"] == "square"
    assert call(square, "perimeter") == 12
    assert call(square, "area") == 9
```

{{% cite SDXPyObjectsAndClasses %}}

Instead of using `obj.method_name(arg)`, `call` looks up the function stored in
the dictionary and calls it with the dictionary as the first object. We can
think of an object as a special kind of dictionary. A method is a function that
takes an object of the right kind as its first parameter (typically called
`self` in Python). {{% cite SDXPyObjectsAndClasses %}}

## Classes

Design goal: objects should store different values, but have the same behaviors,
e.g., squares can have different sizes but must have the same methods.

```py
Square = {
    "_class_name": "Square",
    "perimeter": square_perimeter,
    "area": square_area,
}

def square_new(side):
    return {
        "name": "square",
        "side": side,
        "_class": Square,
    }

def call(thing, method_name):
    return thing["_class"][method_name](thing)

def is_same_class(thing1, thing2):
    return thing1["_class"] == thing2["_class"]
```

{{% cite SDXPyObjectsAndClasses %}}

To support methods that can take a number of arguments, we'd need something like
`call_0`, `call_1`, `call_2`, etc. However, modern languages have affordances
for defining variadic functions, e.g., Python affords us `*` and `**`:

```py
def show_args(first, *args, **kwargs):
  print(f"{first}, args '{args}' and kwargs '{kwargs}'")

show_args("nothing")                  # nothing, args '()' and kwargs '{}'
show_args("one unnamed", 1)           # one unnamed, args '(1,)' and kwargs '{}'
show_args("one named", second=2)      # one named, args '()' and kwargs '{'second': 2}'
show_args("one of each", 4, fourth=4) # one of each, args '(4,)' and kwargs '{'fourth': 4}'

def show_spread(left, middle, right):
  print(f"left={left}, middle={middle}, right={right}")

all_in_list = [1, 2, 3]
all_in_dict = {"right": 3, "left": 1, "middle": 2}

show_spread(*all_in_list)   # left=1, middle=2, right=3
show_spread(**all_in_dict)  # left=1, middle=2, right=3
```

... leading to:

```py
def square_density(sq, mass):
    return mass / square_area(sq)

Square = {
    "_class_name": "Square",
    "perimeter": square_perimeter,
    "area": square_area,
    "density": square_density,
}

def call(thing, method_name, *args, **kwargs):
    return thing["_class"][method_name](thing, *args, **kwargs)

def test():
    square = square_new(4)
    assert call(square, "density", 64) == 4
```

{{% cite SDXPyObjectsAndClasses %}}

## Inheritance

Design goal: Both `Square` and `Circle` implement `density` in the same way due
to both of them being geometric shapes; share this code.

```py
def shape_density(s, mass):
    return mass / call(s, "area")

Shape = {
    "_class_name": "Shape",
    "_parent": None,
    "density": shape_density,
}

Square = {
    "_class_name": "Square",
    "_parent": Shape,
    "perimeter": square_perimeter,
    "area": square_area,
}

def call(thing, method_name, *args, **kwargs):
    method = find_method(thing["_class"], method_name)
    return method(thing, *args, **kwargs)

def find_method(_cls, method_name):
    while _cls is not None:
        if method_name in _cls: return _cls[method_name]
        _cls = _cls["_parent"]

    raise NotImplementedError(f"{method_name}")
```

{{% cite SDXPyObjectsAndClasses %}}

{{% comment %}}

It's the little things that reduce cognitive load. My initial implementation of
`find_method` had:

```py
def find_method(thing, method_name):
    target = thing["_class"]
    while target is not None:
        if method_name in target: return target[method_name]
        target = target["_parent"]

    raise NotImplementedError(f"{method_name}")
```

... but {{% cite SDXPyObjectsAndClasses %}} has a cleaner approach where
`find_method` only deals with classes and traversing up the inheritance
hierarchy.

{{% /comment %}}

## Constructors

Design goal: Make sure that when a square or circle is made, it's made
correctly. Add `_new`, a special member that constructs objects of that type,
e.g.,

```py
def shape_new(name):
    return {
        "name": name,
        "_class": Shape,
    }

Shape = {
    "_class_name": "Shape",
    "_parent": None,
    "_new": shape_new,
    "density": shape_density,
}

def square_new(side):
    return make(Shape, "square") | {
        "side": side,
        "_class": Square,
    }

Square = {
    "_class_name": "Square",
    "_parent": Shape,
    "_new": square_new,
    "perimeter": square_perimeter,
    "area": square_area,
}

def make(_cls, *args, **kwargs):
    return _cls["_new"](*args, **kwargs)
```

{{% cite SDXPyObjectsAndClasses %}}

## Dynamic Dispatch

`find_method`'s dynamic dispatch can be improved by adding a `_cache` dictionary
to each object, where the keys are the names of methods that have been called in
the past, and the values are the functions that were found to implement those
methods. {{% cite SDXPyObjectsAndClasses %}}

C++'s compiler generates a virtual function table, an array of virtual function
pointers. Each virtual function is assigned a fixed integer index. Instances of
that type store a pointer to the vtable as part of their instance data. Because
C++ does not support late binding, the vtable cannot be modified at runtime. At
runtime, calling `shape->area()` is a matter of calling `vptr[i]` where `i` is
the index assigned to `area`. {{% cite DynamicDispatchWiki %}} {{% cite
vTableWiki %}}

## References

1. {{< citation
  id="SDXPyObjectsAndClasses"
  author="Greg Wilson"
  title="Objects and Classes"
  url="https://third-bit.com/sdxpy/oop/"
  accessed="2026-05-16" >}}

1. {{< citation
  id="DynamicDispatchWiki"
  title="Dynamic dispatch - Wikipedia"
  url="https://en.wikipedia.org/wiki/Dynamic_dispatch"
  accessed="2026-05-16" >}}

1. {{< citation
  id="vTableWiki"
  title="Virtual method table - Wikipedia"
  url="https://en.wikipedia.org/wiki/Virtual_method_table"
  accessed="2026-05-16" >}}

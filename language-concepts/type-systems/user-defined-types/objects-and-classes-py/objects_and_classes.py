from math import pi

def shape_density(s, mass):
    return mass / call(s, "area")

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

def square_perimeter(sq):
    return sq["side"] * 4

def square_area(sq):
    return sq["side"] ** 2

def square_density(sq, mass):
    return mass / square_area(sq)

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

def circle_perimeter(ci):
    return pi * ci["radius"] * 2

def circle_area(ci):
    return pi * ci["radius"] * ci["radius"]

def circle_density(sq, mass):
    return mass / circle_area(sq)

def circle_new(radius):
    return make(Shape, "circle") | {
        "radius": radius,
        "_class": Circle,
    }

Circle = {
    "_class_name": "Circle",
    "_parent": Shape,
    "_new": circle_new,
    "perimeter": circle_perimeter,
    "area": circle_area,
}

def call(thing, method_name, *args, **kwargs):
    method = find_method(thing["_class"], method_name)
    return method(thing, *args, **kwargs)

def find_method(_cls, method_name):
    while _cls is not None:
        if method_name in _cls: return _cls[method_name]
        _cls = _cls["_parent"]

    raise NotImplementedError(f"{method_name}")

def is_same_class(thing1, thing2):
    return thing1["_class"] == thing2["_class"]

def make(_cls, *args, **kwargs):
    return _cls["_new"](*args, **kwargs)

import math

from objects_and_classes import make, Square, Circle, call, is_same_class

def test_name():
    square = make(Square, 3)
    circle = make(Circle, 2)

    assert square["name"] == "square"
    assert circle["name"] == "circle"

def test_perimeter():
    square = make(Square, 3)
    circle = make(Circle, 3)

    assert call(square, "perimeter") == 12
    assert call(circle, "perimeter") == math.pi * 6

def test_area():
    square = make(Square, 3)
    circle = make(Circle, 3)

    assert call(square, "area") == 9
    assert call(circle, "area") == math.pi * 9

def test_density():
    square = make(Square, 4)
    circle = make(Circle, 2)

    assert call(square, "density", 64) == 4
    assert call(circle, "density", math.pi) == 0.25

def test_class_identity():
    square = make(Square, 4)
    square_2 = make(Square, 5)
    circle = make(Circle, 2)

    assert is_same_class(square, square_2) == True
    assert is_same_class(square, circle) == False

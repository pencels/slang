f = {
    x y -> x + y
}

f2 = f 2

g = {
    x x -> f2
}

c = {
    1 -> "one"
    2 -> "two"
    3 -> "three"
    _ -> "other"
}

d = {
    1 2 -> "one"
    _ 3 -> "two"
    _ _ -> "other"
}

nest = {
    x 1 -> 1
    x 2 -> 2
    x _ -> 3
    z _ -> 4
    y x -> { 1 -> y + x }
    y w -> { z -> y + w + z }
}
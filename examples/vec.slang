let Vec = {
    :new _x _y -> {
        :x -> _x
        :y -> _y
        :x! x -> _x = x
        :y! y -> _y = y
        :add other -> Vec :new (self :x + other :x) (self :y + other :y)
        :add! other -> {
            self :x! (self :x + other :x)
            self :y! (self :y + other :y)
        }
        :dot other -> self :x * other :x + self :y * other :y
        :to_str -> "<x=" + self :x + ", y=" + self :y + ">"
    }
}

let u = Vec :new 40 50
let v = Vec :new 1 2

print u :add v :to_str
u :add! v
print u :to_str
u :x! 6
print u :to_str
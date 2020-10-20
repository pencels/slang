
add_dispatch {
    :boo x y -> x + y
}

let table = dispatch_table

let foo = (
    add_dispatch {
        :boo 1 1 -> "special :)"
    }

    print 1 :boo 1 --> Uses local row -> "special :)"
    print 1 :boo 2 --> Falls back on first row -> 3

    print table == dispatch_table

    1 :boo
)

print table == dispatch_table

print foo 2 --> 3

-- chains
let foo = chain { x 1 -> x } { y 2 -> y + 1 }
let bar = chain { 3 -> 3 } { 4 -> 4 }

-- composing chains with boxes
let baz = chain { 5 -> 5 } (foo 10)
let qux = chain (foo 99) { _ -> 3 }

print foo 10 2 --> 11
print (foo 10) 2 --> 11
print bar 4 --> 4

print baz 5 --> 5
print baz 2 --> 11

print qux 1 --> 99
print qux 5 --> 3

-- composing chains with chains
let quz = chain (foo 420) ({ x 3 -> x + 300 } 30)
let qix = chain bar (foo 420)

print quz 1 --> 420
print quz 3 --> 330

print qix 4 --> 4
print qix 2 --> 421
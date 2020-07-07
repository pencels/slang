-- Test custom operator declarations.

operator infix left 700 ^
extend_prim :Number {
    a :^ !{b} -> if (b == 0) 1 { a * a ^ (b - 1) }
}

extend_prim :Lazy {
    a :^ b -> *!a ^ b
}

operator prefix ^
let box = {
    val -> [:Box, ref val]
}
extend_prim :List {
    [:Box, r] := val -> r :set val
    :^ [:Box, r] -> r :get
}
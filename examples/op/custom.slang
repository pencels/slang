-- Test custom operator declarations.

operator infix left 700 ^
add_dispatch {
    :^ !{a} !{b} | all is_num [a, b] ->
        if (b == 0) 1 { a * a ^ (b - 1) }
    
    :^ _ _ -> "lol"
}

let box = {
    val -> [:Box, ref val]
}

operator prefix ^
add_dispatch {
    := [:Box, r] val -> r :set val
    :prefix^ [:Box, r] -> r :get
}
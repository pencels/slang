-- Test looping done by prelude fns which leverage
-- full strict eval in sequences / at the top level

let x = 0

let lazy = if_unwrap (x == 0) {
    print "x is zero!"
    { print "this should print later" }
} {
    print "this shouldn't print!"
    { print "this DEFINITELY shouldn't print." }
}

let y = {
    while { x < 20 } {
        if (x % 2 == 0) {
            print "" + x + " is even!"
        } {
            print "" + x + " is odd!"
        }
        x = x + 1
    }
}

print "== Something should print here:"
lazy -- Strict eval.
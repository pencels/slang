-- Test implicit eval done by prelude fns
-- Since `if` and `while` implicitly evaluate their args,
-- we can avoid spamming * and *! everywhere

let x = 0

let lazy = if (x == 0) {
    print "x is zero!"
    { print "this should print later" }
} {
    print "this shouldn't print!"
    { print "this DEFINITELY shouldn't print." }
}

while { x < 20 } {
    if (x % 2 == 0) {
        print "" + x + " is even!"
    } {
        print "" + x + " is odd!"
    }
    x = x + 1
}

print "== Nothing should print here:"
lazy   -- Doesn't cause strict eval.

print "== Something should print here:"
*lazy -- Strict eval.
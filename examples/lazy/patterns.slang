let bar = { print "hello"; { print "world"; 12 } }
let result = nothing
let lazy = nothing

print ""
print "============================================="
print ""

-- >1 non-full strict patterns.
-- A lazy argument should only be strict evaluated once.
let qux = {
    {x} :a -> x
    {x} :b -> x
}

print "Should print 'hello':"
lazy = qux bar :b

print "Should print 'world' and then '12':"
print *lazy

print ""
print "============================================="
print ""

-- >1 full strict patterns.
-- A lazy argument should only be fully strict eval'd once.
let baz = {
    !{x} :a -> x
    !{x} :b -> x
}

print "Should print 'hello', then 'world':"
result = baz bar :b

print "Should print 'true':"
print result == 12


print ""
print "============================================="
print ""

-- Mixed full strict and non-full strict patterns.
-- A lazy argument should only be strict evaluated once, and then
-- fully evaluated from that point on for full strict patterns.
-- The lazy argument should NOT be evaluated all over again.
let foo = {
    {a}  :a -> a
    !{b} :b -> b
    {c}  :c -> c
    !{d} :d -> d
}

print "Should print 'hello', then 'world':"
lazy = foo bar :c

print "Should print 'world':"
result = *lazy

print "Should print 'true':"
print result == 12


print "Should print 'hello', then 'world':"
result = foo bar :d

print "Should print '12':"
print result
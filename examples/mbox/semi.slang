-- Test parsing of semicolon-seprated exprs
-- in the result side of a matchbox.

let effect = {
    1 -> print "hi"; -- comment
         print "bye";
         1,
    2 -> print "henlo world"; 2 -- comment

    3 ->


    print 3, -- comment

    4 -> 4, -- comment
}

print effect 1 -- prints "hi", "bye", then 1
print effect 2 -- prints "henlo world", then 2
print effect 3 -- prints 3, then nothing
print effect 4 -- prints 4



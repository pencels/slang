-- Test assigning lazies. Since strict eval is explicit,
-- we should be able to re-assign lazies without them
-- being prematurely strictified.

let lazy = nothing

lazy = { print "shouldn't see this" }

lazy = lazy

lazy = { print "also shouldn't see this print!" }

lazy = { print "here i am!" }

print "nothing should have printed except this:"
*lazy
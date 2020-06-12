-- Test memoize - evaluate a lazy once and memo its value
let p = memoize { print "hi"; 6 }
let q = memoize { print "bye"; 2 }

print *p --> prints "hi" and returns 6
print *p --> only returns 6

-- Make sure memos are different between memoized lazies
print *q --> prints "bye" and returns 2
print *q --> only returns 2

-- Output should look like:
-- hi
-- 6
-- 6
-- bye
-- 2
-- 2
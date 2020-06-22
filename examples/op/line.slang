print 1 + -- a binary operator can be split across lines
1

-- however, the operator must be trailing on the line.
-- the following parses as: print 1; +1
print 1 
+ 1

nothing
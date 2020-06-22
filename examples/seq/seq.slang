()

( -- comment
    print "one"
    print "two"
    print "three"
)

let x = ( 1 -- can comment within seqs
          2
          3 )

print x --> should be 3

let y = 1; print "y = " + y
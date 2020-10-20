-- Examples of scoped dispatch

print 1 + 1 --> 2

-- Ok let's just throw addition out the window, but just for now
(
    add_dispatch {
        :+ _ _ -> "addition is canceled"
    }
    print 1 + 1 --> "addition is canceled"
)

-- Addition is back to normal here
print 1 + 1 --> 2


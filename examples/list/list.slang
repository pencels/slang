-- Test list parsing

print []

print [1]

print [

    --comment

]

print [ 1 -- comment
        2 -- comment
        3 ]

print [
    1 -- comment
    2 -- comment
    3 -- comment
]

print [ -- comment
    1, -- comment
    2, -- comment
    3, -- comment
]

print [1,2,3]
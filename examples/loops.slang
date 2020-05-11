let times = {
    0 _ -> nothing
    n body -> { body; times (n - 1) body }
}

let while = {
    :false _    -> nothing
    cond body -> { body; while cond body }
}

let x = 0
while { x < 10 } {
    print x
    x = x + 1
}
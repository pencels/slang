let if = {
    :true then _  -> then
    :false _ else -> else
}

let times = {
    0 _    -> nothing
    n body -> { body; times (n - 1) body }
}

let while = {
    :false _  -> nothing
    cond body -> { body; while cond body }
}

let eval = { { x } -> x }

let memoize = {
    let __memo = :None
    {
        x -> {
            if (__memo == :None) {
                let v = eval x
                __memo = [:Some, v]
                v
            } {
                __memo 1
            }
        }
    }
}

let ref = {
    val -> {
        :get -> val
        :set new_val -> val = new_val
    }
}

let map = {
    f []       -> []
    f [h, t..] -> [f h] + map f t
}

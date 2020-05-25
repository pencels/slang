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

let __primitives__ = memoize {
    let atom_bindings = ref {
        atom :to_str -> "" + atom
    }

    let list_bindings = ref {
        list :foreach f -> { map f list; nothing }
        [] :to_str -> "[]"
        list :to_str -> {
            let suffix = {
                [] -> "]"
                [h, t..] -> ("" + h + ",") + suffix t
            }

            "[" + suffix list
        }
        [] idx -> "index out of bounds exception"
        list idx ->
    }

    let number_bindings = ref {
        num :plus other -> num + other
    }

    {
        :Atom -> atom_bindings
        :List -> list_bindings
        :Nothing -> ref nothing
        :Number -> number_bindings
        :String -> ref nothing
    }
}

let extend_prim = {
    type extension -> __primitives__ type :set (__primitives__ type :get + extension)
}

extend_prim :List { list :map f -> map f list }

-- extend Option types
extend_prim :Atom {
    :None :get -> "error"
    :None :get_or alt -> alt
}
extend_prim :List {
    [:Some, x] :get -> x
    [:Some, x] :get_or _ -> x
}

let if = {
    :true then _  -> then
    :false _ else -> else
}

let if_unwrap = {
    :true then _  -> *then
    :false _ else -> *else
}

let times = {
    0 _    -> nothing
    n body -> body; times (n - 1) body
}

let while = {
    :false _  -> nothing
    cond body -> body; while cond body
}

let loop = {
    body -> body; loop body
}

let eval = { { x } -> x }
-- Could also be written like:
-- let eval = { x -> *x }

let eval_full = { !{ x } -> x }
-- Could also be written like:
-- let eval_full = { x -> *!x }

let memoize = {
    let memo = :None
    {
        lazy -> {
            if_unwrap (memo == :None) {
                let v = *lazy
                memo = [:Some, v]
                v
            } {
                let [_, v] = memo
                v
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
        _ :type -> "Atom"
    }

    let list_bindings = ref {
        list :map f -> map f list
        list :foreach f -> list :map { x -> *!(f x) }; nothing

        []       :len -> 0
        [_, t..] :len -> 1 + t :len

        [] :empty? -> :true
        _  :empty? -> :false

        [] :mk_str start _ end -> "" + start + end
        [h, t..] :mk_str start delim end -> (
            let suffix = {
                [] -> "" + end
                [h, t..] -> ("" + delim + h) + suffix t
            }

            "" + start + h + suffix t
        )
        list :to_str -> list :mk_str "[" "," "]"

        [] :try_at _ -> :None
        [h, t..] :try_at 0 -> [:Some, h]
        [h, t..] :try_at n -> t :try_at (n - 1)

        list :at n -> list :try_at n :get_or "index out of bounds exception"

        _ :type -> "List"
    }

    let nothing_bindings = ref {
        _ :type -> "Nothing"
    }

    let number_bindings = ref {
        start :to end -> if (start > end) [] { [start] + (start + 1) :to end }

        0 :times _ -> nothing
        n :times body -> body; (n - 1) :times body

        _ :type -> "Number"
    }

    let string_bindings = ref {
        _ :type -> "String"
    }

    {
        :Atom -> atom_bindings
        :List -> list_bindings
        :Nothing -> nothing_bindings
        :Number -> number_bindings
        :String -> string_bindings
    }
}

let extend_prim = {
    type extension -> __primitives__ type :set (extension + __primitives__ type :get)
}

-- extend Option types
extend_prim :Atom {
    :None :get -> "error"
    :None :get_or alt -> alt
}
extend_prim :List {
    [:Some, x] :get -> x
    [:Some, x] :get_or _ -> x
}

nothing
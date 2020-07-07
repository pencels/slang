operator prefix + - ! * *!
operator infix left 300 @
operator infix left 400 < <= > >= == !=
operator infix left 500 + -
operator infix left 600 * / %
operator postfix !

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
            {
                :None -> (
                    let v = __strict__ lazy
                    memo = [:Some, v]
                    v
                )
                [:Some, v] -> v
            } memo
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
        :! :true -> :false
        :! :false -> :true
        :* x -> x
        :*! x -> x
        a :@ m -> m a
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

        :* x -> x
        :*! x -> x

        l :+ r -> __concat_list__ l r
        l :@ m -> m l
        _ :type -> "List"
    }

    let nothing_bindings = ref {
        :* x -> x
        :*! x -> x
        n :@ m -> m n
        _ :type -> "Nothing"
    }

    let number_bindings = ref {
        start :to end -> if (start > end) [] { [start] + *((start + 1) :to end) }

        :+ n -> __to_num__ n
        :- n -> __neg__ n

        a :+ b -> __add__ a *!b
        a :- b -> __sub__ a *!b

        a :* b -> __mul__ a *!b
        a :/ b -> __div__ a *!b
        a :% b -> __mod__ a *!b

        a :< b -> __lt__ a *!b
        a :<= b -> __le__ a *!b
        a :> b -> __gt__ a *!b
        a :>= b -> __ge__ a *!b
        a :== b -> __eq__ a *!b
        a :!= b -> __ne__ a *!b

        :* x -> x
        :*! x -> x

        0 :! -> 1
        n :! -> n * (n - 1)!

        0 :times _ -> nothing
        n :times body -> body; (n - 1) :times body

        n :@ m -> m n
        _ :type -> "Number"
    }

    let string_bindings = ref {
        :* x -> x
        :*! x -> x

        s :+ other -> __concat_str__ s *!other

        a :== b -> __eq__ a *!b
        a :!= b -> __ne__ a *!b

        s :@ m -> m s
        _ :type -> "String"
    }
    
    let lazy_bindings = ref {
        :* l -> __strict__ l
        :*! l -> __full_strict__ l
        l :@ m -> m l
    }

    {
        :Atom -> atom_bindings
        :List -> list_bindings
        :Nothing -> nothing_bindings
        :Number -> number_bindings
        :String -> string_bindings
        :Lazy -> lazy_bindings
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
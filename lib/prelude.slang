operator prefix + - ! * *!
operator infix left 300 @
operator infix left 400 < <= > >= == !=
operator infix left 433 ||
operator infix left 466 &&
operator infix left 500 ++ ~ + -
operator infix left 600 * / % `div` *~
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

let unwrap = { { x } -> x }
let unravel = { !{ x } -> x }

let thunk = {
    let memo = []
    {
        lazy -> {
            {
                [] -> (
                    let v = unwrap lazy
                    memo = [v]
                    v
                )
                [v] -> v
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

let all = {
    _ [] -> :true
    cond (h . t) -> cond h && all cond t
}

let any = {
    _ [] -> :false
    cond (h . t) -> cond h || any cond t
}

let is_atom = { val -> type val == "Atom" }
let is_lazy = { val -> type val == "Lazy" }
let is_list = { val -> type val == "List" }
let is_num = { val -> type val == "Number" }
let is_string = { val -> type val == "String" }

dispatch {
    :prefix! :true -> :false
    :prefix! :false -> :true

    :&& :false _ -> :false
    :&& :true :true -> :true

    :|| :true _ -> :true
    :|| :false :false -> :false

    :== x y -> __eq__ *!x *!y
    :!= x y -> !(x == y)

    :prefix+ n -> __to_num__ *!n
    :prefix- n -> __neg__ *!n

    :postfix! 0 -> 1
    :postfix! n -> n * (n - 1)!

    :+ a b -> __add__ *!a *!b
    :- a b -> __sub__ *!a *!b

    :* a b | all is_num [a, b] -> __mul__ *!a *!b
    :/ a b -> __fdiv__ *!a *!b
    :div a b -> __div__ *!a *!b
    :% a b -> __mod__ *!a *!b

    :< a b -> __lt__ *!a *!b
    :> a b -> __gt__ *!a *!b
    :<= a b -> !(a > b)
    :>= a b -> !(a < b)

    :++ a b -> __concat_list__ *!a *!b
    :~ a b -> __concat_str__ *!a *!b

    :*~ 0 s | is_string s -> ""
    :*~ n s | is_num n && is_string s -> s ~ (n - 1) *~ s

    :prefix*   {x} -> x
    :prefix*! !{x} -> x

    :@ arg matchbox -> matchbox arg

    :map [] _ -> []
    :map (h . t) f -> f h . t :map f

    :foreach list f | is_list list -> list :map { x -> *!(f x) }; nothing

    :len [] -> 0
    :len (_ . t) -> 1 + t :len

    :empty? [] -> :true
    :empty? _ -> :false

    :mk_str [] start _ end -> "" ~ start ~ end
    :mk_str (h . t) start delim end -> (
        let suffix = {
            [] -> "" ~ end
            h . t -> ("" ~ delim ~ h) ~ suffix t
        }

        "" ~ start ~ h ~ suffix t
    )

    :to_str list | is_list list -> list :mk_str "[" ", " "]"

    :try_at [] _ -> :None
    :try_at (h . t) 0 -> [:Some, h]
    :try_at (_ . t) n -> t :try_at (n - 1)

    :at list n | is_list list && is_num n ->
        list :try_at n :get_or "index out of bounds exception"

    :to start end | all is_num [start, end] ->
        if_unwrap (start > end) [] { start . (start + 1) :to end }

    :times 0 _ -> nothing
    :times n body -> body; (n - 1) :times body
}

nothing
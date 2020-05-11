let if = {
    :true then _ -> then
    :false _ else -> else
}

let List = {
    :map f :nil -> :nil
    :map f list -> {
        let newHead = f (list :h)
        let newTail = self :map f (list :t)
        self :cons newHead newTail
    }

    :cons h t -> {
        :h -> h
        :t -> t

        :append x -> {
            t @ {
                :nil -> List :cons h (List :cons x :nil)
                _ -> List :cons h (t :append x)
            }
        }

        :concat :nil -> self
        :concat other -> self :append (other :h) :concat (other :t)

        :to_str -> List :to_str self
    }

    :to_str :nil -> "[]"
    :to_str list -> {
        let inner_to_str = {
            :nil -> "]"
            list -> ", " + list :h + inner_to_str (list :t)
        }

        "[" + list :h + inner_to_str (list :t)
    }
}

let list123 = List :cons 1 (List :cons 2 (List :cons 3 :nil))
let list456 = List :cons 4 (List :cons 5 (List :cons 6 :nil))

let add1 = List :map {x -> x + 1}

let Range = {
    :from i -> self :from (i + 1)
    :between i j -> nothing
}

let Counter = {
    :new x -> {
        :next -> {
            x = x + 1
            self
        }
        :val -> x
    }
}
let c = Counter :new 0


let What = {
    :new -> {
        let outer = self
        {
            :what -> {
                print self :do
                print outer :do
            }
            :do -> "inner"
        }
    }
    :do -> "outer"
}

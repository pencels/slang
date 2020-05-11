let eval = {
    { true } -> "yes"
    { false } -> "no"
}

let if = {
    :true {x} _ -> x
    :false _ {x} -> x
}


let picky = {
    x 1 -> :true
}

picky {print "hi"; 2}
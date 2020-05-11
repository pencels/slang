let true = { fst snd | fst }
let false = { fst snd | snd }
let if = { cond then else |
    cond then else
}

if true {
    print "hello"
} {
    print "goodbye"
}


let times = { n thing |
    if (n <= 0) {
        nothing
    } {
        times (n - 1) thing
    }
}
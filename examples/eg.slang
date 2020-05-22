let g = {
    x y 1 -> [x, y]
    x w 2 -> [x, w]
    z 1 q -> [z, q]
    1 2 5 -> 4
}

let f = {
    :x 0 -> 1
    :x 1 -> 2
}

let h = {
    cond -> {
        print "should print before h"
        cond
    }
}
h { print "h" }
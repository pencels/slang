if = {
    :true {then} _ -> then
    :false _ {else} -> else
}

lookup = {
    [] _ -> nothing
    [[k_, v], rest..] k -> {
        if (k_ == k) { v } { lookup rest k }
    }
}


data = [
    ["a", 0],
    ["b", 2]
]

print lookup data "a"
--> 0

print lookup data "d"
--> nothing


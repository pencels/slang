let place_char = {
    n | n <= 0 -> nothing
    1 -> ["I", "V"]
    2 -> ["X", "L"]
    3 -> ["C", "D"]
    4 -> ["M", "W"]
    n -> ["M" ~ (n - 4) *~ "'", "W" ~ (n - 4) *~ "'"]
}

let roman_place = {
    _ n | n <= 0 -> ""
    place n -> (
        let [unit, half] = place_char place
        let [whole, _] = place_char (place + 1)
        let suffix = (n % 10) @ {
            0 -> ""
            4 -> unit ~ half
            9 -> unit ~ whole
            n | n >= 5 -> half ~ (n - 5) *~ unit
            n | n >= 1 -> n *~ unit
        }
        roman_place (place + 1) (n `div` 10) ~ suffix
    )
}

let roman = roman_place 1


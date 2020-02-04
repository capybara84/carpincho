module List

fun length x =
    if x == [] then
        0
    else
        1 + length (tl x)



fun length x =
    if x == [] then
        0
    else
        1 + length (tl x)

let x = length [1,2,3,4,5]
puts "length [1,2,3,4,5] = "
putn x
nl ()


fun fact n =
    if n < 1 then 1
    else n * fact (n-1)

fun loop s e =
    if s < e then {
        puts "fact "; putn s; puts " = "; putn (fact s); nl()
        loop (s+1) e
    } else ()

loop 1 10

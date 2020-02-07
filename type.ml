
open Syntax

let seed = ref 0

let new_unknown () =
    let ty = TVar (!seed, ref None, ref false) in
    incr seed;
    ty



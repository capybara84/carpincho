open Syntax

let error s = raise (Error ("Runtime error: " ^ s))

let fn_nl _ =
    print_newline ();
    flush stdout;
    VUnit

let fn_putn = function
    | VInt n -> print_int n; VUnit
    | _ -> error "type error ('int' required)"

let fn_puts = function
    | VString s -> print_string s; VUnit
    | _ -> error "type error ('string' required)"

let fn_head = function
    | VCons (hd, _) -> hd
    | _ -> error "type error ('list' required)"

let fn_tail = function
    | VCons (_, tl) -> tl
    | _ -> error "type error ('list' required)"

let add_fn name func env =
    Env.extend name (ref (VBuiltin func)) env

let init () =
    let env = add_fn "nl" fn_nl [] in
    let env = add_fn "putn" fn_putn env in
    let env = add_fn "puts" fn_puts env in
    let env = add_fn "hd" fn_head env in
    let env = add_fn "tl" fn_tail env in
    env


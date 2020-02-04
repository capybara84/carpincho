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

let fn_desc v =
    describe_value v;
    VUnit

let show_sym (id,v) = print_endline (" " ^ id ^ " = " ^ value_to_string !v)

let fn_modules _ =
    let show_env (id,symtab) =
        print_endline ("module " ^ id);
        List.iter show_sym symtab.env
    in
    List.iter show_env @@ Symbol.get_all_modules ();
    VUnit

let fn_env _ =
    let tab = Symbol.get_current_module () in
    List.iter show_sym tab.env;
    VUnit

let init env =
    let add_fn name func env =
        Env.extend name (ref (VBuiltin func)) env
    in
    let env = add_fn "nl" fn_nl env in
    let env = add_fn "putn" fn_putn env in
    let env = add_fn "puts" fn_puts env in
    let env = add_fn "hd" fn_head env in
    let env = add_fn "tl" fn_tail env in
    let env = add_fn "desc" fn_desc env in
    let env = add_fn "_mod" fn_modules env in
    let env = add_fn "_env" fn_env env in
    env


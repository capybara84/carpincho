open Syntax

let error s = raise (Error ("Runtime error: " ^ s))
let type_error s = error ("type '" ^ s ^ "' required")

let fn_nl _ =
    print_newline ();
    flush stdout;
    VUnit

let fn_putn = function
    | VInt n -> print_int n; VUnit
    | _ -> type_error "int"

let fn_puts = function
    | VString s -> print_string s; VUnit
    | _ -> type_error "string"

let fn_head = function
    | VCons (hd, _) -> hd
    | _ -> type_error "list"

let fn_tail = function
    | VCons (_, tl) -> tl
    | _ -> type_error "list"

let fn_first = function
    | VTuple (x::_) -> x
    | _ -> type_error "tuple"

let fn_second = function
    | VTuple (_::x::_) -> x
    | _ -> type_error "tuple"

let fn_describe v =
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

let builtin_list = [
    ("nl", fn_nl);
    ("putn", fn_putn);
    ("puts", fn_puts);
    ("hd", fn_head);
    ("tl", fn_tail);
    ("fst", fn_first);
    ("snd", fn_second);
    ("desc", fn_describe);
    ("modules", fn_modules);
    ("env", fn_env);
]

let init () =
    let add_func (name, fn) =
        Symbol.insert_default name (VBuiltin fn)
    in
    List.iter add_func builtin_list


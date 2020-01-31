
open Syntax

type t = {
    mutable tokens : token list;
}

let debug = ref 0
let debug_indent = ref 0
let rec debug_show_space n =
    if n = 0 then ()
    else begin
        print_char ' ';
        debug_show_space (n-1)
    end
let debug_print_indent level s =
    if !debug < level then ()
    else begin
        debug_show_space !debug_indent;
        print_endline s
    end
let debug_print_in level s =
    if !debug < level then ()
    else begin
        debug_show_space !debug_indent;
        incr debug_indent;
        print_endline ("IN " ^ s)
    end
let debug_print_out level s =
    if !debug < level then ()
    else begin
        decr debug_indent;
        debug_show_space !debug_indent;
        print_endline ("OUT " ^ s)
    end
let debug_print level s =
    if !debug < level then ()
    else
        print_endline s

let debug_parse_in msg = debug_print_in 3 msg
let debug_parse_out msg = debug_print_out 3 msg
let debug_parse msg = debug_print_indent 3 msg
let debug_token msg = debug_print 2 msg


let error pars msg =
    raise (Error msg)

let peek_token pars =
    match pars.tokens with
    | [] -> {token_type=EOF;line=0}
    | tok::_ -> tok


and parse_expr pars =
    debug_parse_in "parse_expr";
    let e = match peek_token_type pars with
        | EOF -> Eof
        | NEWLINE | SEMI -> next_token pars; parse_expr pars
        | LET -> parse_let pars
        | FN -> parse_fn pars
        | IF -> parse_if pars
        | BEGIN -> parse_compound pars
        | _ -> parse_logical pars
    in
    if peek_token_type pars = SEMI then
        next_token pars
    else
        ();
    debug_parse_out "parse_expr";
    e

let parse tokens =
    let pars = { tokens = tokens } in
    debug_token ("token = " ^ (token_to_string @@ peek_token pars));
    let rec loop res =
        let e = parse_expr pars in
        if e = Eof then
            List.rev res
        else
            loop (e::res)
    in
    loop []


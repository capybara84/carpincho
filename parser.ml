
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

let token_to_binop = function
    | PLUS -> BinAdd
    | MINUS -> BinSub
    | STAR -> BinMul
    | SLASH -> BinDiv
    | PERCENT -> BinMod
    | LT -> BinLT
    | LE -> BinLE
    | GT -> BinGT
    | GE -> BinGE
    | EQL -> BinEql
    | NEQ -> BinNeq
    | LOR -> BinLor
    | LAND -> BinLand
    | COLON -> BinCons
    | _ -> failwith "binop bug"

let token_to_unop = function
    | NOT -> UNot
    | MINUS -> UMinus
    | _ -> failwith "unop bug"

let is_unop = function
    | NOT | MINUS -> true | _ -> false

let is_mul_op = function
    | STAR | SLASH | PERCENT -> true | _ -> false

let is_add_op = function
    | PLUS | MINUS -> true | _ -> false

let is_equal_op = function
    | LT | LE | GT | GE | EQL | NEQ -> true | _ -> false

let is_logical_op = function
    | LOR | LAND -> true | _ -> false

let is_apply e t =
    match e with
    | Fn _ | Apply _ | Ident _ | IdentMod _ ->
        begin
            match t with
            | EMPTY | NULL | ID _ | BOOL_LIT _ | INT_LIT _ | CHAR_LIT _
            | STRING_LIT _ | LPAR | LBRA -> true
            | _ -> false
        end
    | _ -> false

let peek_token pars =
    match pars.tokens with
    | [] -> {token_type=EOF;line=0}
    | tok::_ -> tok

let peek_token_type pars =
    let token = peek_token pars in
    token.token_type

let next_token pars =
    begin
        match pars.tokens with
        | [] -> ()
        | _::toks -> pars.tokens <- toks
    end;
    debug_token ("token = " ^ (token_to_string @@ peek_token pars))

let rec expect pars token_type =
    let current_type = peek_token_type pars in
    if current_type = token_type then
        next_token pars
    else if current_type = NEWLINE then
        (next_token pars; expect pars token_type)
    else
        error pars ("missing token '" ^ token_type_to_string token_type ^
                "' at '" ^ token_type_to_string current_type ^ "'")

let rec expect_id pars =
    match peek_token_type pars with
    | ID id -> next_token pars; id
    | NEWLINE -> next_token pars; expect_id pars
    | t -> error pars ("missing identifier at '" ^ token_type_to_string t ^ "'")

let rec skip_newline pars =
    match peek_token_type pars with
    | NEWLINE ->
        next_token pars; skip_newline pars
    | _ -> ()

let rec parse_list_expr pars =
    debug_parse_in "parse_list_expr";
    let lhs = parse_expr pars in
    let rhs = 
        match peek_token_type pars with
        | COMMA -> begin
            next_token pars;
            skip_newline pars;
            parse_list_expr pars
          end
        | _ -> Null
    in
    debug_parse_out "parse_list_expr";
    Binary (BinCons, lhs, rhs)

and parse_simple pars =
    debug_parse_in "parse_simple";
    let res =
        match peek_token_type pars with
        | EOF -> Eof
        | EMPTY ->
            next_token pars;
            Unit
        | NULL ->
            next_token pars;
            Null
        | ID id ->
            next_token pars;
            if peek_token_type pars = DOT then begin
                next_token pars;
                let id2 = expect_id pars in
                IdentMod (id, id2)
            end else
                Ident id
        | BOOL_LIT b ->
            next_token pars;
            BoolLit b
        | INT_LIT i ->
            next_token pars;
            IntLit i
        | CHAR_LIT c ->
            next_token pars;
            CharLit c
        | STRING_LIT s ->
            next_token pars;
            StrLit s
        | LPAR ->
            next_token pars;
            skip_newline pars;
            let e = parse_expr pars in
            let rec loop lst =
                let e = parse_expr pars in
                if peek_token_type pars = COMMA then begin
                    next_token pars;
                    skip_newline pars;
                    loop (e::lst)
                end else
                    List.rev (e::lst)
            in
            if peek_token_type pars = COMMA then begin
                next_token pars;
                skip_newline pars;
                let e2 = loop [] in
                expect pars RPAR;
                Tuple (e::e2)
            end else begin
                expect pars RPAR;
                e
            end
        | LBRA ->
            next_token pars;
            skip_newline pars;
            let e = parse_list_expr pars in
            expect pars RBRA;
            (*TODO calc list size, if 0 then it is NULL *)
            e
        | t ->
            next_token pars;
            error pars ("syntax error at '" ^ token_type_to_string t ^ "'")
    in
    debug_parse_out "parse_simple";
    res

and parse_unary pars =
    debug_parse_in "parse_unary";
    let op = peek_token_type pars in
    let res =
    if is_unop op then begin
        next_token pars;
        let e = parse_simple pars in
        Unary (token_to_unop op, e)
    end else
        parse_simple pars
    in
    debug_parse_out "parse_unary";
    res

and parse_apply pars =
    debug_parse_in "parse_apply";
    let rec parse_apply_rhs lhs =
        let a = parse_simple pars in
        let e = Apply (lhs, a) in
        if is_apply e (peek_token_type pars) then
            parse_apply_rhs e
        else
            e
    in
    let e = parse_unary pars in
    let res = if is_apply e (peek_token_type pars) then
        parse_apply_rhs e
    else
        e
    in
    debug_parse_out "parse_apply";
    res

and parse_mul pars =
    debug_parse_in "parse_mul";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if not (is_mul_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_apply pars in
            parse_rhs (Binary (op, lhs, rhs))
        end
    in
    let e = parse_apply pars in
    let e = parse_rhs e
    in
    debug_parse_out "parse_mul";
    e

and parse_add pars =
    debug_parse_in "parse_add";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if not (is_add_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_mul pars in
            parse_rhs (Binary (op, lhs, rhs))
        end
    in
    let e = parse_mul pars in
    let e = parse_rhs e
    in
    debug_parse_out "parse_add";
    e

and parse_cons pars =
    debug_parse_in "parse_cons";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if tt <> COLON then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_add pars in
            Binary (op, lhs, parse_rhs rhs)
        end
    in
    let e = parse_add pars in
    let e = parse_rhs e
    in
    debug_parse_out "parse_cons";
    e

and parse_equal pars =
    debug_parse_in "parse_equal";
    let lhs = parse_cons pars in
    let tt = peek_token_type pars in
    let e =
    if not (is_equal_op tt) then
        lhs
    else begin
        let op = token_to_binop tt in
        next_token pars;
        let rhs = parse_cons pars in
        Binary (op, lhs, rhs)
    end
    in
    debug_parse_out "parse_equal";
    e

and parse_logical pars =
    debug_parse_in "parse_logical";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if not (is_logical_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_equal pars in
            parse_rhs (Binary (op, lhs, rhs))
        end
    in
    let e = parse_equal pars in
    let e = parse_rhs e
    in
    debug_parse_out "parse_logical";
    e

and parse_decl_list pars =
    debug_parse_in "parse_decl_list";
    let rec loop () =
        match peek_token_type pars with
            | EOF | END -> []
            | _ ->
                begin
                    let e = parse_decl pars in
                    skip_newline pars;
                    e::(loop ())
                end
    in
    let e = loop () in
    debug_parse_in "parse_decl_list";
    e

and parse_compound pars =
    debug_parse_in "parse_compound";
    next_token pars;
    skip_newline pars;
    let e = parse_decl_list pars in
    expect pars END;
    skip_newline pars;
    debug_parse_out "parse_compound";
    Comp e

and parse_if pars =
    debug_parse_in "parse_if";
    next_token pars;
    skip_newline pars;
    let e1 = parse_expr pars in
    expect pars THEN;
    skip_newline pars;
    let e2 = parse_expr pars in
    expect pars ELSE;
    skip_newline pars;
    let e3 = parse_expr pars in
    debug_parse_out "parse_if";
    If (e1, e2, e3)

and parse_param_list pars args =
    debug_parse_in "parse_param_list";
    let e =
        match peek_token_type pars with
        | WILDCARD ->
            next_token pars;
            parse_param_list pars (WildCard::args)
        | ID id ->
            let id = expect_id pars in
            parse_param_list pars (Ident id::args)
        | _ ->
            List.rev args
    in
    debug_parse_out "parse_param_list";
    e

and parse_params pars =
    debug_parse_in "parse_params pars";
    let e =
        if peek_token_type pars = EMPTY then
            (next_token pars; [Unit])
        else
            parse_param_list pars []
    in
    debug_parse_out "parse_params pars";
    e

and parse_fn pars =
    debug_parse_in "parse_fn";
    next_token pars;
    skip_newline pars;
    let args = parse_params pars in
    expect pars ARROW;
    skip_newline pars;
    let e = List.fold_right (fun arg body -> Fn (arg, body))
                        args (parse_decl pars) in
    debug_parse_out "parse_fn";
    e

and parse_pattern_list pars =
    debug_parse_in "parse_pattern_list";
    let rec loop res =
        if peek_token_type pars <> COMMA then
            List.rev res
        else begin
            next_token pars;
            skip_newline pars;
            let e = parse_pattern pars in
            loop (e::res)
        end
    in
    let e = parse_pattern pars in
    let e = loop [e] in
    debug_parse_in "parse_pattern_list";
    e

and parse_a_pattern pars =
    debug_parse_in "parse_a_pattern";
    let e =
        match peek_token_type pars with
        | NULL -> next_token pars; PatNull
        | WILDCARD -> next_token pars; PatWildCard
        | ID id -> next_token pars; PatIdent id
        | BOOL_LIT b -> next_token pars; PatBool b
        | INT_LIT i -> next_token pars; PatInt i
        | CHAR_LIT c -> next_token pars; PatChar c
        | STRING_LIT s -> next_token pars; PatStr s
        | LPAR ->
            next_token pars;
            skip_newline pars;
            let pl = parse_pattern_list pars in
            expect pars RPAR;
            if List.length pl = 1 then
                List.hd pl
            else
                PatTuple pl
        | LBRA ->
            next_token pars;
            skip_newline pars;
            let pl = parse_pattern_list pars in
            expect pars RBRA;
            if List.length pl == 0 then
                PatNull
            else
                PatList pl
        | t ->
            next_token pars;
            error pars ("syntax error at '" ^ token_type_to_string t ^ "'")
    in
    debug_parse_out "parse_a_pattern";
    e

and parse_cons_pattern pars =
    debug_parse_in "parse_cons_pattern";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if tt <> COLON then
            lhs
        else begin
            next_token pars;
            let rhs = parse_a_pattern pars in
            PatCons (lhs, parse_rhs rhs)
        end
    in
    let e = parse_a_pattern pars in
    let e = parse_rhs e in
    debug_parse_out "parse_cons_pattern";
    e

and parse_or_pattern pars =
    debug_parse_in "parse_or_pattern";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if tt <> OR then
            lhs
        else begin
            next_token pars;
            let rhs = parse_cons_pattern pars in
            PatOr (lhs, parse_rhs rhs)
        end
    in
    let e = parse_cons_pattern pars in
    let e = parse_rhs e in
    debug_parse_out "parse_or_pattern";
    e

and parse_pattern pars =
    debug_parse_in "parse_pattern";
    if peek_token_type pars = OR then
        next_token pars
    else ();
    let e = parse_or_pattern pars in
    let e =
        if peek_token_type pars = AS then begin
            next_token pars;
            PatAs (e, expect_id pars)
        end else
            e
    in
    debug_parse_out "parse_pattern";
    e

and parse_match_list pars =
    debug_parse_in "parse_match_list";
    let rec loop res =
        match peek_token_type pars with
        | EOF | END -> res
        | _ ->
            begin
                let p = parse_pattern pars in
                expect pars ARROW;
                skip_newline pars;
                let e = parse_expr pars in
                skip_newline pars;
                if peek_token_type pars == OR then begin
                    next_token pars;
                    loop ((p, e) :: res)
                end else
                    (p, e) :: res
            end
    in
    let lst = loop [] in
    debug_parse_out "parse_match_list";
    List.rev lst

and parse_match pars =
    debug_parse_in "parse_match";
    next_token pars;
    let e = parse_expr pars in
    expect pars BEGIN;
    skip_newline pars;
    let lst = parse_match_list pars in
    expect pars END;
    debug_parse_out "parse_match";
    Match (e, lst)

and parse_expr pars =
    debug_parse_in "parse_expr";
    let e = match peek_token_type pars with
        | EOF -> Eof
        | NEWLINE | SEMI -> next_token pars; parse_expr pars
        | FN -> parse_fn pars
        | IF -> parse_if pars
        | MATCH -> parse_match pars
        | BEGIN -> parse_compound pars
        | _ -> parse_logical pars
    in
    if peek_token_type pars = SEMI then
        next_token pars
    else
        ();
    debug_parse_out "parse_expr";
    e

and parse_let pars =
    debug_parse_in "parse_let";
    next_token pars;
    skip_newline pars;
    let id = expect_id pars in
    expect pars EQ;
    skip_newline pars;
    let e = parse_expr pars in
    skip_newline pars;
    debug_parse_out "parse_let";
    Let (id, e)

and parse_fun pars =
    debug_parse_in "parse_fun";
    next_token pars;
    skip_newline pars;
    let id = expect_id pars in
    let args = parse_params pars in
    expect pars EQ;
    skip_newline pars;
    let e = List.fold_right (fun arg body -> Fn (arg, body))
                            args (parse_expr pars)
    in
    debug_parse_out "parse_fun";
    LetRec (id, e)

and parse_decl pars =
    debug_parse_in "parse_decl";
    let e = match peek_token_type pars with
        | EOF -> Eof
        | NEWLINE | SEMI -> next_token pars; parse_decl pars
        | LET -> parse_let pars
        | FUN -> parse_fun pars
        | _ -> parse_expr pars
    in
    if peek_token_type pars = SEMI then
        next_token pars
    else
        ();
    debug_parse_out "parse_decl";
    e

let parse_import pars =
    debug_parse_in "parse_import";
    next_token pars;
    let id = expect_id pars in
    let rename =
        if peek_token_type pars = AS then
            (next_token pars; Some (expect_id pars))
        else
            None
    in
    debug_parse_out "parse_import";
    Import (id, rename)

let parse_module pars =
    debug_parse_in "parse_module";
    next_token pars;
    let id = expect_id pars in
    debug_parse_out "parse_module";
    Module id

let rec parse_top_level pars =
    debug_parse_in "parse_top_level";
    let e = match peek_token_type pars with
        | EOF -> Eof
        | NEWLINE | SEMI -> next_token pars; parse_top_level pars
        | MODULE -> parse_module pars
        | IMPORT -> parse_import pars
        | _ -> parse_decl pars
    in
    debug_parse_out "parse_top_level";
    e

let parse_one tokens =
    let pars = { tokens = tokens } in
    debug_token ("token = " ^ (token_to_string @@ peek_token pars));
    parse_top_level pars

let parse tokens =
    let pars = { tokens = tokens } in
    debug_token ("token = " ^ (token_to_string @@ peek_token pars));
    let rec loop res =
        let e = parse_top_level pars in
        if e = Eof then
            List.rev res
        else
            loop (e::res)
    in
    loop []


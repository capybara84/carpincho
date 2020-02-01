
open Syntax

type t = {
    source : string;
    len : int;
    mutable pos : int;
    mutable line : int;
}


let is_end scan =
    scan.pos = scan.len

let peek scan =
    if is_end scan then
        None
    else
        Some scan.source.[scan.pos]

let next_char scan =
    if is_end scan then
        ()
    else
        scan.pos <- scan.pos + 1

let next_line scan =
    next_char scan;
    scan.line <- scan.line + 1

let cut_token pred scan =
    let buffer = Buffer.create 5 in
    let rec loop () =
        match peek scan with
        | Some ch when pred ch ->
            begin
                Buffer.add_char buffer ch;
                next_char scan;
                loop ()
            end
        | _ -> ()
    in loop ();
    Buffer.contents buffer

let scan_number scan =
    let is_digit = function '0'..'9' -> true | _ -> false in
    INT_LIT (int_of_string (cut_token is_digit scan)) 

let scan_ident scan =
    let is_ident = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
                    | _ -> false in
    let id = cut_token is_ident scan in
    match id with
    | "_" -> WILDCARD
    | "let" -> LET
    | "fn" -> FN
    | "fun" -> FUN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "true" -> BOOL_LIT true
    | "false" -> BOOL_LIT false
    | _ -> ID id

let get_char scan =
    match peek scan with
    | Some '\\' ->
        begin
            next_char scan;
            match peek scan with
            | Some 'n' -> '\n'
            | Some 'r' -> '\r'
            | Some 't' -> '\t'
            | Some ch -> ch
            | None -> raise (Error "unexpected EOF");
        end
    | Some ch -> ch
    | None -> raise (Error "unexpected EoF")

let scan_char scan =
    next_char scan;
    let c = get_char scan in
    let res = CHAR_LIT c in
    next_char scan;
    match peek scan with
    | Some '\'' ->
        (next_char scan; res)
    | Some _ -> raise (Error "missing single-quote")
    | None -> raise (Error "Unexpected EOF")

let scan_string scan =
    next_char scan;
    let buffer = Buffer.create 10 in
    let rec loop () =
        match peek scan with
        | Some '"' ->
            next_char scan
        | None ->
            raise (Error "unexpected eof")
        | _ ->
            begin
                let c = get_char scan in
                Buffer.add_char buffer c;
                next_char scan;
                loop ()
            end
    in
    loop ();
    STRING_LIT (Buffer.contents buffer)

let rec skip_newline scan =
    next_line scan;
    match peek scan with
    | Some '\n' -> skip_newline scan
    | Some '#' -> skip_comment scan
    | _ -> NEWLINE

and skip_comment scan =
    next_char scan;
    match peek scan with
    | Some ch when ch <> '\n' -> skip_comment scan
    | _ -> skip_newline scan

let rec scan_token scan =
    let scan_token2 ch token_type2 token_type1 =
        next_char scan;
        match peek scan with
        | Some c when c = ch ->
            (next_char scan; token_type2)
        | _ -> token_type1
    in
    match peek scan with
    | None -> EOF
    | Some ' ' | Some '\t' | Some '\r' -> next_char scan; scan_token scan
    | Some '\n' -> skip_newline scan
    | Some '0'..'9' -> scan_number scan
    | Some 'a'..'z' | Some 'A'..'Z' | Some '_' -> scan_ident scan
    | Some '\'' -> scan_char scan
    | Some '"' -> scan_string scan
    | Some '#' -> skip_comment scan
    | Some ']' -> next_char scan; RBRA
    | Some ')' -> next_char scan; RPAR
    | Some '{' -> next_char scan; BEGIN
    | Some '}' -> next_char scan; END
    | Some ',' -> next_char scan; COMMA
    | Some '+' -> next_char scan; PLUS
    | Some '/' -> next_char scan; SLASH
    | Some '*' -> next_char scan; STAR
    | Some '%' -> next_char scan; PERCENT
    | Some ':' -> next_char scan; COLON
    | Some ';' -> next_char scan; SEMI
    | Some '=' -> scan_token2 '=' EQL EQ
    | Some '!' -> scan_token2 '=' NEQ NOT
    | Some '<' -> scan_token2 '=' LE LT
    | Some '>' -> scan_token2 '=' GE GT
    | Some '-' -> scan_token2 '>' ARROW MINUS
    | Some '(' -> scan_token2 ')' UNIT LPAR
    | Some '[' -> scan_token2 ']' NULL LBRA
    | Some '|' ->
        next_char scan;
        if peek scan = Some '|' then (next_char scan; LOR)
        else raise (Error ("illegal character '|'"))
    | Some '&' ->
        next_char scan;
        if peek scan = Some '&' then (next_char scan; LAND)
        else raise (Error ("illegal character '&'"))
    | Some c ->
        begin
            next_char scan;
            raise (Error ("illegal character '" ^ String.make 1 c ^ "'"))
        end

let get_tokens scan =
    let make_token tok_type = { token_type = tok_type; line = scan.line } in
    let rec loop result =
        let token_type = scan_token scan in
        match token_type with
        | EOF -> (make_token token_type)::result
        | _ -> loop ((make_token token_type)::result)
    in 
    List.rev (loop [])

let from_string s =
    get_tokens {
        source = s;
        len = String.length s;
        pos = 0;
        line = 1;
    }

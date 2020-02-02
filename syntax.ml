
exception Error of string

type token_type
    = EOF | NEWLINE | ID of string | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | STRING_LIT of string
    | LET | FN | FUN | IF | THEN | ELSE
    | EQ | EQL | NEQ | LT | LE | GT | GE
    | MINUS | PLUS | SLASH | STAR | PERCENT
    | NOT | UNIT | LOR | LAND
    | ARROW | LPAR | RPAR | LBRA | RBRA | BEGIN | END
    | WILDCARD | COMMA | NULL | COLON | SEMI

type token = {
    token_type : token_type;
    line : int;
}

type ident = string

type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod
    | BinLT | BinLE | BinGT | BinGE | BinEql | BinNeq
    | BinLor | BinLand | BinCons

type unop = UNot | UMinus

type expr =
    | Eof | Unit | Null | WildCard
    | BoolLit of bool | IntLit of int | CharLit of char | StrLit of string
    | Ident of ident | Binary of binop * expr * expr
    | Unary of unop * expr
    | Apply of expr * expr
    | Let of ident * expr
    | LetRec of ident * expr
    | Fn of expr * expr
    | If of expr * expr * expr
    | Comp of expr list

type typ =
    | TUnit | TBool | TInt | TChar | TString
    | TList of typ
    | TFun of typ * typ
    | TVar of int * typ option ref

type value =
    | VUnit
    | VNull
    | VBool of bool
    | VInt of int
    | VChar of char
    | VString of string
    | VCons of value * value
    | VClosure of expr * expr * (value ref) Env.t
    | VBuiltin of (value -> value)

let token_type_to_string = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>"
    | ID id -> id | BOOL_LIT true -> "true" | BOOL_LIT false -> "false"
    | INT_LIT n -> string_of_int n | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'"
    | STRING_LIT s -> "\"" ^ s ^ "\""
    | LET -> "let" | FN -> "fn" | FUN -> "fun" | IF -> "if" | THEN -> "then"
    | ELSE -> "else" | EQ -> "=" | EQL -> "==" | NEQ -> "!=" | LT -> "<"
    | LE -> "<=" | GT -> ">" | GE -> ">=" | MINUS -> "-" | PLUS -> "+"
    | SLASH -> "/" | STAR -> "*" | PERCENT -> "%"
    | NOT -> "!" | UNIT -> "()" | LOR -> "||" | LAND -> "&&"
    | ARROW -> "->" | LPAR -> "(" | RPAR -> ")" | LBRA -> "[" | RBRA -> "]"
    | BEGIN -> "{" | END -> "}" | WILDCARD -> "_" | COMMA -> ","
    | NULL -> "[]" | COLON -> ":" | SEMI -> ";"

let token_to_string t = token_type_to_string t.token_type

let string_of_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*"
    | BinDiv -> "/" | BinMod -> "%" | BinLT -> "<"
    | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">="
    | BinEql -> "==" | BinNeq -> "!=" | BinLor -> "||"
    | BinLand -> "&&" | BinCons -> ":"

let string_of_unop = function
    | UNot -> "!"
    | UMinus -> "-"

let rec expr_to_string = function
    | Eof -> "<Eof>"
    | Unit -> "()"
    | Null -> "[]"
    | WildCard -> "_"
    | BoolLit true -> "true"
    | BoolLit false -> "false"
    | IntLit n -> string_of_int n
    | CharLit c -> "'" ^ String.make 1 c ^ "'"
    | StrLit s -> "\"" ^ s ^ "\""
    | Ident id -> id
    | Binary (op, lhs, rhs) ->
        "(" ^ expr_to_string lhs ^ " " ^ string_of_binop op ^ " "
            ^ expr_to_string rhs ^ ")"
    | Unary (op, e) ->
        "(" ^ string_of_unop op ^ expr_to_string e ^ ")"
    | Apply (e1, e2) ->
        "(" ^ expr_to_string e1 ^ " " ^ expr_to_string e2 ^ ")"
    | Let (id, e) ->
        "(let " ^ id ^ " = " ^ expr_to_string e ^ ")"
    | LetRec (id, e) ->
        "(letrec " ^ id ^ " " ^ expr_to_string e ^ ")"
    | Fn (e1, e2) ->
        "(fn " ^ expr_to_string e1 ^ " -> " ^ expr_to_string e2 ^ ")"
    | If (e1, e2, e3) ->
        "(if " ^ expr_to_string e1 ^ " then " ^ expr_to_string e2 ^ " else "
        ^ expr_to_string e3 ^ ")"
    | Comp el ->
        "{" ^ comp_to_string el ^ "}"
and
    comp_to_string = function
    | [] -> ""
    | x::xs -> expr_to_string x ^ "; " ^ comp_to_string xs

(*
let rec exprs_to_string = function
    | [] -> ""
    | x::xs -> expr_to_string x ^ "; " ^ exprs_to_string xs
*)

let rec value_to_string = function
    | VUnit -> "()"
    | VNull -> "[]"
    | VBool true -> "true"
    | VBool false -> "false"
    | VInt n -> string_of_int n
    | VChar c -> String.make 1 c
    | VString s -> s
    | VCons (_, VCons _) as e -> "[" ^ value_list_to_string e ^ "]"
    | VCons (car, VNull) -> "[" ^ value_to_string car ^ "]"
    | VCons (car, cdr) -> value_to_string car ^ ":" ^ value_to_string cdr
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"
and value_list_to_string = function
    | VCons (lhs, rhs) ->
        begin match rhs with
        | VNull -> value_to_string lhs
        | VCons (_,_) -> value_to_string lhs ^ ", " ^ value_list_to_string rhs
        | _ -> failwith "list rhs bug"
        end
    | _ -> failwith "list bug"


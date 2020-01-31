
exception Error of string

type token_type
    = EOF | NEWLINE | ID of string | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | STRING_LIT of string
    | LET | FN | IF | THEN | ELSE
    | EQ | EQL | NEQ | LT | LE | GT | GE
    | MINUS | PLUS | SLASH | STAR | PERCENT
    | NOT | UNIT | LOR | LAND
    | ARROW | LPAR | RPAR | LBRA | RBRA | BEGIN | END
    | WILDCARD | COMMA | NULL | COLON | SEMI

type token = {
    token_type : token_type;
    line : int;
}

let token_type_to_string = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>"
    | ID id -> id | BOOL_LIT true -> "true" | BOOL_LIT false -> "false"
    | INT_LIT n -> string_of_int n | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'"
    | STRING_LIT s -> "\"" ^ s ^ "\""
    | LET -> "let" | FN -> "fn" | IF -> "if" | THEN -> "then" | ELSE -> "else"
    | EQ -> "=" | EQL -> "==" | NEQ -> "!=" | LT -> "<" | LE -> "<="
    | GT -> ">" | GE -> ">=" | MINUS -> "-" | PLUS -> "+"
    | SLASH -> "/" | STAR -> "*" | PERCENT -> "%"
    | NOT -> "!" | UNIT -> "()" | LOR -> "||" | LAND -> "&&"
    | ARROW -> "->" | LPAR -> "(" | RPAR -> ")" | LBRA -> "[" | RBRA -> "]"
    | BEGIN -> "{" | END -> "}" | WILDCARD -> "_" | COMMA -> ","
    | NULL -> "[]" | COLON -> ":" | SEMI -> ";"

let token_to_string t = token_type_to_string t.token_type

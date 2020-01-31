
open Syntax

let test_text = "
identifier 12345
'a' '\\t' \"abc\\n\"
let fn if then else
= == != < <= > >=
- + / * %
! () || &&
-> ( ) [ ] { }
_ , [] : ;
"

let test_tokens = [
    NEWLINE; ID "identifier"; INT_LIT 12345; NEWLINE;
    CHAR_LIT 'a'; CHAR_LIT '\t'; STRING_LIT "abc\n"; NEWLINE;
    LET; FN; IF; THEN; ELSE; NEWLINE;
    EQ; EQL; NEQ; LT; LE; GT; GE; NEWLINE;
    MINUS; PLUS; SLASH; STAR; PERCENT; NEWLINE;
    NOT; UNIT; LOR; LAND; NEWLINE;
    ARROW; LPAR; RPAR; LBRA; RBRA; BEGIN; END; NEWLINE;
    WILDCARD; COMMA; NULL; COLON; SEMI; NEWLINE;
    EOF
]

let scanner_test verbose =
    try
        let tokens = Scanner.from_string test_text in
        let len_tt = List.length test_tokens in
        let len_t = List.length tokens in
        Test.equal len_tt len_t ("length " ^ string_of_int len_tt
                                    ^ " != " ^ string_of_int len_t);
        if verbose then
            List.iter
                (fun x ->
                    print_endline ("[" ^ token_type_to_string x.token_type
                                    ^ ", " ^ string_of_int x.line ^ "]"))
                tokens
        else ();
        List.iter2
            (fun tt t -> Test.equal tt t.token_type
                ((token_type_to_string tt) ^ " != " ^ (token_to_string t)))
            test_tokens tokens
    with Invalid_argument s -> Test.fail ("Invalid_argument:" ^ s)


let init verbose =
    Test.add "Scanner" verbose scanner_test



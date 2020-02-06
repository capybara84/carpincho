
open Syntax

let print_test = [
    (TTuple [TInt;TInt], "(int, int)");
    (TTuple [TInt;TInt;TInt], "(int, int, int)");
    (TTuple [TTuple [TInt;TInt];TInt], "((int, int), int)");
    (TTuple [TInt;TTuple [TInt;TInt]], "(int, (int, int))");
    (TTuple [TTuple [TInt;TInt];TTuple [TInt;TInt]], "((int, int), (int, int))");
    (TFun (TInt,TInt), "int -> int");
    (TFun (TInt, TFun (TInt, TInt)), "int -> int -> int");
    (TFun (TFun (TInt, TInt), TInt), "(int -> int) -> int");
    (TFun (TInt, TTuple [TInt; TInt]), "int -> (int, int)");
    (TFun (TTuple [TInt; TInt], TInt), "(int, int) -> int");
    (TList TInt, "[int]");
    (TList (TTuple [TInt;TInt;TInt]), "[(int, int, int)]");
    (TList (TFun (TInt,TInt)), "[int -> int]");
    (TList (TFun (TFun (TInt,TInt),TInt)), "[(int -> int) -> int]");
]

let type_print_test verbose =
    if verbose then
        print_newline()
    else ();
    List.iter
        (fun (t,s) ->
            let r = type_to_string t in
            if verbose then begin
                print_endline ("result: " ^ r);
                print_endline ("expect: " ^ s)
            end else ();
            Test.equal r s ("result: " ^ r ^ " != " ^ s ^ "\n"))
        print_test

let init verbose =
    Test.add "Type Print" verbose type_print_test

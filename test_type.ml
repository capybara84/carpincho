
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

let all_exprs = [
    ("'c'", "char");
    ("\"abc\"", "string");
    ("12", "int");
    ("300+12", "int");
    ("300*12+3", "int");
    ("300*(12+3)", "int");
    ("fn x -> x + 1", "int -> int");
    ("fn _ -> ()", "'a -> unit");
    ("(fn x -> x + 1) (300 * (12 + 3))", "int");
    ("1+2 < 3*4", "bool");
    ("2 * -(1+2)", "int");
    ("fn x -> fn y -> x + y", "'a -> 'a -> 'a");
    ("(fn x -> x) 1", "int");
    ("(fn x -> x) 1==1", "bool");
    ("(fn _ -> 1) 'a'", "int");
    ("(fn _ -> 2) 3", "int");

    ("[]", "['a]");
    ("[1,2,3]", "[int]");
    ("1:2:[]", "[int]");
    ("['a','b']", "[char]");

    ("(fn x -> x)", "'a -> 'a");
    ("fn x -> fn y -> x", "'a -> 'b -> 'a");
    ("fn x -> fn y -> y", "'a -> 'b -> 'b");
    ("(fn x -> x + 1) 2 + (fn x -> x + -1) 3", "int");
    ("fn f -> fn g -> fn x -> g (f x)", "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c");
    ("fn x -> fn y -> fn z -> x z (y z)", "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c");
    ("fn x -> {let y = x + 1; x}", "int -> int");
    ("fn x -> {let y = x + 1; y}", "int -> int");
    ("fn b -> fn x -> if x b then x else (fn x -> b)",
        "bool -> (bool -> bool) -> bool -> bool");
    ("fn x -> if true then x else (if x then true else false)", "bool -> bool");
    ("fn x -> fn y -> if x then x else y", "bool -> bool -> bool");
    ("fn n -> (fn x -> x (fn y -> y)) (fn f -> f n)", "'a -> 'a");
    ("fn x -> fn y -> x y", "('a -> 'b) -> 'a -> 'b");
    ("fn x -> fn y -> x (y x)", "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b");
    ("fn x -> fn y -> x (y x) (y x)", "('a -> 'a -> 'b) -> (('a -> 'a -> 'b) -> 'a) -> 'b");
    ("fn x -> fn y -> fn z -> x (z x) (y (z x y))", "((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> (((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> 'a) -> 'c");
    ("{ let id = fn x -> x; let f = fn y -> id (y id); f}", "(('a -> 'a) -> 'b) -> 'b"); 
    ("{ let k = fn x -> fn y -> x; let k1 = fn x -> fn y -> k (x k); k1 }",
        "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let s1 = fn x -> fn y -> fn z -> x s (z s) (y s (z s)); s1 }",
        "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f");
    ("{ let g = fn h -> fn t -> fn f -> fn x -> f h (t f x); g }",
        "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let k = fn x -> fn y -> x; let kk = fn x -> fn y -> x; s k kk }", "'a -> 'a");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let k = fn x -> fn y -> x; s k k }",
        "'a -> 'a");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let kk = fn x -> fn y -> y; s kk kk }",
        "'a -> 'b -> 'b");

    (* TODO 
    ("fn x -> fn y -> fn z -> { let b = x y z ; if b then z y else y }",
        "('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a");
    *)

    ("{let pair = fn x1 -> fn x2 -> fn y -> y x1 x2; let proj1 = fn p -> p (fn x1 -> fn x2 -> x1); let proj2 = fn p -> p (fn x1 -> fn x2 -> x2); proj1 (pair 1 100)}", "int");
    ("{let pair = fn x1 -> fn x2 -> fn y -> y x1 x2; let proj1 = fn p -> p (fn x1 -> fn x2 -> x1); let proj2 = fn p -> p (fn x1 -> fn x2 -> x2); proj1 (proj2 (pair 10 (pair 20 30)))}", "int");
    ("{let f = fn x -> x; if f true then f 1 else f 2}", "int");
    ("{let f = fn x -> 3; f true + f 4}", "int");
    ("fn b -> {let f = fn x -> x; let g = fn y -> y; if b then f g else g f}", "bool -> 'a -> 'a");
    (*TODO
    ("fn b -> fn f -> { let g1 = fn x -> x f; let g2 = fn x -> x f; fn z -> if b then g1 z g2 else g2 z g1}", "bool -> 'a -> ('a -> (('a -> 'b) -> 'b) -> 'c) -> 'c");
    *)

    ("fn x -> match x { [] -> 0 | _ -> 1}", "['a] -> int");
    ("fn x -> match x { true -> 0 | _ -> 2}", "bool -> int");
    ("fn x -> match x { \"abc\" -> 0 | _ -> 2}", "string -> int");
    ("fn x -> match x { x -> x+1 }", "int -> int");
    ("fn x -> match x { x -> x }", "'a -> 'a");
    ("fn x -> match x { (a,b) -> a }", "('a, 'b) -> 'a");
    ("fn x -> match x { (a,b) -> b }", "('a, 'b) -> 'b");
    ("fn x -> match x { [1] -> 0 }", "[int] -> int");
(*
    ("fn x -> match x { [1,2] -> 0 | _ -> 1}", "[int] -> int");
*)
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

let infer_top e =
    let tab = Symbol.get_current_module () in
    let (tenv, t) = Type.infer tab.tenv e in
    Symbol.set_current_tenv tenv;
    t

let type_test verbose =
    let do_test (text, expected) =
        try
            if verbose then
                print_endline ("text> " ^ text)
            else ();
            let t = infer_top @@ Parser.parse_one @@ Scanner.from_string text in
            let s = type_to_string t in
            if verbose then begin
                print_endline ("infer    > " ^ s);
                print_endline ("expected > " ^ expected)
            end else ();
            Test.equal s expected ("result:" ^ s ^ " != " ^ expected ^ "\n")
        with Error s -> Test.fail s
    in
    List.iter do_test all_exprs

let init verbose =
    Test.add "Type Print" verbose type_print_test;
    Test.add "Type" verbose type_test


open Syntax

let all_exprs = [
    ("'a'", CharLit 'a');
    ("\"abc\"", StrLit "abc");
    ("12", IntLit 12);
    ("300 + 12",
        Binary (BinAdd, IntLit 300, IntLit 12));
    ("300 * 12 + 3",
        Binary (BinAdd, Binary (BinMul, IntLit 300, IntLit 12), IntLit 3));
    ("300 * (12 + 3)",
        Binary (BinMul, IntLit 300,
                Binary (BinAdd, IntLit 12, IntLit 3)));
    ("1 / 2 < 3 * 4",
        Binary (BinLT, (Binary (BinDiv, IntLit 1, IntLit 2)),
                        (Binary (BinMul, IntLit 3, IntLit 4))));
    ("2 * -(1 + 2)",
        Binary (BinMul, IntLit 2,
            (Unary (UMinus, Binary (BinAdd, IntLit 1, IntLit 2)))));
    ("5 % 2",
        Binary (BinMod, IntLit 5, IntLit 2));
    ("a && b",
        Binary (BinLand, Ident "a", Ident "b"));
    ("a || b",
        Binary (BinLor, Ident "a", Ident "b"));
    ("!(x < y)",
        Unary (UNot, Binary (BinLT, Ident "x", Ident "y")));
    ("1 <= 2",
        Binary (BinLE, IntLit 1, IntLit 2));
    ("1 > 2",
        Binary (BinGT, IntLit 1, IntLit 2));
    ("1 >= 2",
        Binary (BinGE, IntLit 1, IntLit 2));
    ("1 == 2",
        Binary (BinEql, IntLit 1, IntLit 2));
    ("1 != 2",
        Binary (BinNeq, IntLit 1, IntLit 2));
    ("fn x -> x + 1",
        Fn (Ident "x", Binary (BinAdd, Ident "x", IntLit 1)));
    ("f 3",
        Apply (Ident "f", IntLit 3));
    ("-(f 3)",
        Unary (UMinus, Apply (Ident "f", IntLit 3)));
    ("f (-3)",
        Apply (Ident "f", Unary (UMinus, IntLit 3)));
    ("f -3",
        Binary (BinSub, Ident "f", IntLit 3));
    ("fn () -> 1",
        Fn (Unit, IntLit 1));
    ("(fn x -> x + 1) (300 * (12 + 3))",
        Apply (Fn (Ident "x", Binary (BinAdd, Ident "x", IntLit 1)), 
            Binary (BinMul, IntLit 300,
                Binary (BinAdd, IntLit 12, IntLit 3))));
    ("let fact = fn n -> if n < 1 then 1 else n * fact (n - 1)",
            Let (("fact", TVar (0, {contents = None}, ref false)),
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", IntLit 1),
                          IntLit 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                IntLit 1))))))));
    ("{}",
        Comp []);
    ("{1; 2; }",
        Comp [IntLit 1; IntLit 2]);
    ("{1; 2; 3}",
        Comp [IntLit 1; IntLit 2; IntLit 3]);
    ("let fact = fn n -> if n < 1 then 1 else n * fact (n - 1)",
        Let (("fact", TVar (1, {contents = None}, ref false)),
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", IntLit 1),
                          IntLit 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                IntLit 1))))))));
    ("1+2+3",
        (Binary (BinAdd,
            (Binary (BinAdd, IntLit 1, IntLit 2)), IntLit 3)));
    ("f 1 2",
        Apply (Apply (Ident "f", IntLit 1), IntLit 2));
    ("f 1 2 3",
        (Apply (Apply (Apply (Ident "f",
            IntLit 1), IntLit 2), IntLit 3)));
    ("1:2:3:[]",
        (Binary (BinCons, IntLit 1,
                 (Binary (BinCons, IntLit 2,
                          (Binary (BinCons, IntLit 3, Null)))))));
    ("[1,2,3]",
        (Binary (BinCons, IntLit 1,
                 (Binary (BinCons, IntLit 2,
                          (Binary (BinCons, IntLit 3, Null)))))));
    ("(1)", IntLit 1);
    ("true", BoolLit true);
    ("false", BoolLit false);
    ("fun one () = 1",
        LetRec (("one", TVar (2, {contents=None}, ref false)),
            (Fn (Unit, IntLit 1))));
    ("fun fact n = if n < 1 then 1 else n * fact (n-1)",
        LetRec (("fact", TVar (3, {contents=None}, ref false)),
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", IntLit 1),
                          IntLit 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                IntLit 1))))))));
    ("module List", Module "List");
    ("import Array", Import ("Array", None));
    ("import Array as A", Import ("Array", Some "A"));
    ("Array.length", IdentMod ("Array", "length"));
    ("(1)", IntLit 1);
    ("(1,2)", Tuple [IntLit 1; IntLit 2]);
    ("(1,2,3)", Tuple [IntLit 1; IntLit 2; IntLit 3]);
    ("match x { 0 -> 'a' | 1 -> 'b' | 2 -> 'c' }",
        Match (Ident "x",
                [(PatInt 0, CharLit 'a');
                 (PatInt 1, CharLit 'b');
                 (PatInt 2, CharLit 'c')]));
    ("match x { [] -> 0 | _:xs -> 1 }",
        Match (Ident "x",
                [(PatNull, IntLit 0);
                 (PatCons (PatWildCard, PatIdent "xs"), IntLit 1)]));
    ("match 1 { x -> x }",
        Match (IntLit 1,
                [(PatIdent "x", Ident "x")]));
    ("match n { 1 | 2 | 3 -> 0 | 4 -> 1 }",
        Match (Ident "n",
                [(PatOr (PatInt 1, PatOr (PatInt 2, PatInt 3)), IntLit 0);
                 (PatInt 4, IntLit 1)]));
    ("match n { _ as a -> 1}",
        Match (Ident "n",
                [(PatAs (PatWildCard, "a"), IntLit 1)]));
    ("match n { [a,b,c] -> 3 }",
        Match (Ident "n",
                [(PatList [PatIdent "a"; PatIdent "b"; PatIdent "c"], IntLit 3)]));
    ("match n { (1) -> 2 }",
        Match (Ident "n",
                [(PatInt 1, IntLit 2)]));
    ("match n { (1, 'c') -> 3 }",
        Match (Ident "n",
                [(PatTuple [PatInt 1; PatChar 'c']), IntLit 3]));
    ("match n { (_,_,x) -> x }",
        Match (Ident "n",
                [(PatTuple [PatWildCard; PatWildCard; PatIdent "x"], Ident "x")]));
]

let parser_test verbose =
    let do_parse_test (text, expected) =
        try
            if verbose then 
                print_endline ("text    > " ^ text)
            else ();
            let expr = Parser.parse_one @@ Scanner.from_string text in
            let parsed = expr_to_string expr in
            let expected = expr_to_string expected in
            if verbose then begin
                print_endline ("parsed  > " ^ parsed);
                print_endline ("expected> " ^ expected)
            end else ();
            Test.equal parsed expected
                ("result:" ^ parsed ^ " != " ^ expected ^ "\n")
        with Error s -> Test.fail s
    in
    List.iter do_parse_test all_exprs

let init verbose =
    Test.add "Parser" verbose parser_test





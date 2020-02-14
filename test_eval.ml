
open Syntax

let all_exprs = [
    ("'a'", VChar 'a');
    ("\"abc\"", VString "abc");
    ("12", VInt 12);
    ("300 + 12", VInt 312);
    ("300 * 12 + 3", VInt 3603);
    ("300 * (12 + 3)", VInt 4500);
    ("300 / (12 - 3)", VInt 33);
    ("300 % (12 - 3)", VInt 3);
    ("1 < 2", VBool true);
    ("1 <= 1", VBool true);
    ("1 > 2", VBool false);
    ("2 >= 2", VBool true);
    ("2 == 2", VBool true);
    ("2 == 1", VBool false);
    ("2 != 1", VBool true);
    ("2 != 2", VBool false);
    ("'a' == 'a'", VBool true);
    ("'a' == 'b'", VBool false);
    ("'a' != 'a'", VBool false);
    ("'a' != 'b'", VBool true);
    ("'a' < 'b'", VBool true);
    ("'b' < 'a'", VBool false);
    ("'a' <= 'a'", VBool true);
    ("'b' <= 'a'", VBool false);
    ("'a' > 'b'", VBool false);
    ("'b' > 'a'", VBool true);
    ("'a' >= 'b'", VBool false);
    ("'a' >= 'a'", VBool true);
    ("\"abc\" + \"def\"", VString "abcdef");
    ("\"abc\" == \"abc\"", VBool true);
    ("\"abc\" == \"def\"", VBool false);
    ("\"abc\" != \"abc\"", VBool false);
    ("\"abc\" != \"def\"", VBool true);
    ("\"abc\" < \"def\"", VBool true);
    ("\"abc\" > \"def\"", VBool false);
    ("1 > 2 || 2 > 1", VBool true);
    ("1 < 2 && 2 < 1", VBool false);
    ("-5", VInt (-5));
    ("!true", VBool false);
    ("!false", VBool true);
    ("1:[2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("1:2:[3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3] == 1:[2,3]", VBool true);
    ("[1,2,3] == 1:[2,3,4]", VBool false);
    ("let x = 1", VUnit);
    ("x", VInt 1);
    ("let f = fn () -> 5", VUnit);
    ("f ()", VInt 5);
    ("let g = fn _ -> 8", VUnit);
    ("g 3", VInt 8);
    ("let a = fn x -> x + 1", VUnit);
    ("a 4", VInt 5);
    ("let add = fn x -> fn y -> x + y", VUnit);
    ("add 1 2", VInt 3);
    ("let add5 = add 5", VUnit);
    ("add5 3", VInt 8);
    ("fun foo x = x + 2", VUnit);
    ("foo 4", VInt 6);
    ("fun fact n = if n < 1 then 1 else n * fact (n-1)", VUnit);
    ("fact 5", VInt 120);
    ("module A", VUnit);
    ("let x = 1", VUnit);
    ("module B", VUnit);
    ("let x = 2", VUnit);
    ("module Main", VUnit);
    ("A.x", VInt 1);
    ("B.x", VInt 2);
    ("import List", VUnit);
    ("List.length [1,2,3]", VInt 3);
    ("import List as L", VUnit);
    ("L.length [1,2,3,4]", VInt 4);
    ("fst (1,2)", VInt 1);
    ("snd (1,2)", VInt 2);
    ("(fn n -> match n { 0 -> 'a' | 1 -> 'b' | 2 -> 'c' }) 1", VChar 'b');
    ("(fn n -> match n { (_,_,x) -> x }) (1,2,3)", VInt 3);
    ("(fn n -> match n { 0 | 1 | 2 -> 'a' | 3 -> 'b' }) 1", VChar 'a');
    ("(fn n -> match n { 0 | 1 | 2 -> 'a' | 3 -> 'b' }) 2", VChar 'a');
    ("(fn n -> match n { 0 | 1 | 2 -> 'a' | 3 -> 'b' }) 3", VChar 'b');
    ("match [1,2,3] { [a,b,c] -> a }", VInt 1);
    ("match [1,2,3] { [a,b,c] as d -> a }", VInt 1);
    ("match [1,2,3] { [a,b,c] as d -> b }", VInt 2);
    ("match [1,2,3] { [a,b,c] as d -> c }", VInt 3);
    ("match [1,2,3] { [a,b,c] as d -> d }",
        VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("match [1,2,3] { x:xs -> x }", VInt 1);
    ("match [1,2,3] { x:y:xs -> y }", VInt 2);
    ("match [1,2,3] { x:xs -> xs }", VCons(VInt 2, VCons (VInt 3, VNull)));
]

let eval_test verbose =
    let do_eval_test (text, expected) =
        try
            if verbose then
                print_endline ("text> " ^ text)
            else ();
            let (v, t) = Eval.eval_line text in
            if verbose then begin
                print_endline ("evaluated> " ^ value_to_string v ^ " : "
                                    ^ type_to_string t);
                print_endline ("expected > " ^ value_to_string expected)
            end else ();
            Test.equal v expected
                ("result:" ^ value_to_string v ^ " != "
                    ^ value_to_string expected ^ "\n")
        with Error s -> Test.fail s
    in
    List.iter do_eval_test all_exprs

let init verbose =
    Test.add "Eval" verbose eval_test


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
    ("1:[2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("1:2:[3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("let x = 1", VUnit);
    ("x", VInt 1);
]

let eval_test verbose =
    let env = ref [] in
    let do_eval_test (text, expected) =
        try
            if verbose then
                print_endline ("text> " ^ text)
            else ();
            let (new_env, v) =
                Eval.eval_decl !env @@ Parser.parse_one @@ Scanner.from_string text
            in
            env := new_env;
            if verbose then begin
                print_endline ("evaluated> " ^ value_to_string v);
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

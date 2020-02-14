
open Syntax

let error msg = raise (Error ("Runtime error: " ^ msg))

let default_directory = "./"
let make_module_filename name =
    default_directory ^ String.uncapitalize name ^ ".cp"

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let is_true = function
    | VBool b -> b
    | _ -> error "type error (boolean)"

let eval_unary = function
    | (UMinus, VInt n) -> VInt (-n)
    | (UNot, VBool b) -> VBool (not b)
    | _ -> error "type error (unary)"

let rec eval_equal = function
    | (VUnit, VUnit) -> true
    | (VNull, VNull) -> true
    | (VBool l, VBool r) -> (l = r)
    | (VInt l, VInt r) -> (l = r)
    | (VChar l, VChar r) -> (l = r)
    | (VString l, VString r) -> (l = r)
    | ((VCons _ as lhs), (VCons _ as rhs)) -> list_equal (lhs, rhs)
    | (VCons _, VNull) | (VNull, VCons _) -> false
    | _ -> error "type error (equal)"
and list_equal = function
    | (VNull, VNull) -> true
    | (VCons _, VNull) -> false
    | (VNull, VCons _) -> false
    | (VCons (lh, lt), VCons (rh, rt)) ->
        if not (eval_equal (lh, rh)) then false
        else list_equal (lt, rt)
    | _ -> failwith "list_equal bug"

let eval_binary = function
    | (BinEql, vl, vr) -> VBool (eval_equal (vl, vr))
    | (BinNeq, vl, vr) -> VBool (not (eval_equal (vl, vr)))
    | (BinAdd, VInt l, VInt r) -> VInt (l + r)
    | (BinSub, VInt l, VInt r) -> VInt (l - r)
    | (BinMul, VInt l, VInt r) -> VInt (l * r)
    | (BinDiv, VInt l, VInt r) -> VInt (l / r)
    | (BinMod, VInt l, VInt r) -> VInt (l mod r)
    | (BinLT, VInt l, VInt r) -> VBool (l < r)
    | (BinLE, VInt l, VInt r) -> VBool (l <= r)
    | (BinGT, VInt l, VInt r) -> VBool (l > r)
    | (BinGE, VInt l, VInt r) -> VBool (l >= r)
    | (BinLT, VChar l, VChar r) -> VBool (l < r)
    | (BinLE, VChar l, VChar r) -> VBool (l <= r)
    | (BinGT, VChar l, VChar r) -> VBool (l > r)
    | (BinGE, VChar l, VChar r) -> VBool (l >= r)
    | (BinAdd, VString l, VString r) -> VString (l ^ r)
    | (BinLT, VString l, VString r) -> VBool (l < r)
    | (BinLE, VString l, VString r) -> VBool (l <= r)
    | (BinGT, VString l, VString r) -> VBool (l > r)
    | (BinGE, VString l, VString r) -> VBool (l >= r)
    | (BinCons, vl, vr) -> VCons (vl, vr)
    | _ -> error "type error (binary)"

let rec eval_expr env = function
    | Eof | Unit -> VUnit
    | Null -> VNull
    | BoolLit b -> VBool b
    | IntLit n -> VInt n
    | CharLit c -> VChar c
    | StrLit s -> VString s
    | Ident id ->
        (try
            !(Env.lookup id env)
        with Not_found ->
            (try
                !(Symbol.lookup_default id)
            with Not_found -> error("'" ^ id ^ "' not found")))
    | IdentMod (mod_name, id) ->
        (try
            !(Symbol.lookup mod_name id)
        with Not_found -> error("'" ^ mod_name ^ "." ^ id ^ "' not found"))
    | Tuple el ->
        VTuple (List.map (fun x -> eval_expr env x) el)
    | Binary (BinLor, lhs, rhs) ->
        let vl = eval_expr env lhs in
        if is_true vl then
            VBool true
        else
            eval_expr env rhs
    | Binary (BinLand, lhs, rhs) ->
        let vl = eval_expr env lhs in
        if not (is_true vl) then
            VBool false
        else
            eval_expr env rhs
    | Binary (op, lhs, rhs) ->
        let vl = eval_expr env lhs in
        let vr = eval_expr env rhs in
        eval_binary (op, vl, vr)
    | Unary (op, e) ->
        let v = eval_expr env e in
        eval_unary (op, v)
    | Apply (e1, e2) ->
        let fun_part = eval_expr env e1 in
        let arg_part = eval_expr env e2 in
        begin
            match fun_part with
            | VClosure (Ident x, body, old_env) ->
                let new_env = Env.extend x (ref arg_part) old_env in
                eval_expr new_env body
            | VClosure (WildCard, body, old_env) ->
                eval_expr env body
            | VClosure (Unit, body, old_env) ->
                eval_expr env body
            | VBuiltin fn ->
                fn arg_part
            | v -> error ("application of non-function: " ^ value_to_string v)
        end
    | Fn (x, e) ->
        VClosure (x, e, env)
    | If (e1, e2, e3) ->
        let vc = eval_expr env e1 in
        let v = eval_expr env (if is_true vc then e2 else e3)
        in v
    | Match (e, ml) ->
        let v = eval_expr env e in
        eval_match env v ml
    | Comp el ->
        let (_, v) = eval_list env el in
        v
    | _ -> failwith ("eval bug")

and eval_match env v match_list =
    let rec match_with env = function
        | (PatNull, VNull) -> (env, true)
        | (PatNull, _) -> (env, false)
        | (PatWildCard, _) -> (env, true)
        | (PatBool b, VBool vb) when b = vb -> (env, true)
        | (PatBool _, _) -> (env, false)
        | (PatInt i, VInt vi) when i = vi -> (env, true)
        | (PatInt _, _) -> (env, false)
        | (PatChar c, VChar vc) when c = vc -> (env, true)
        | (PatChar _, _) -> (env, false)
        | (PatStr s, VString vs) when s = vs -> (env, true)
        | (PatStr _, _) -> (env, false)
        | (PatIdent id, v) -> (Env.extend id (ref v) env, true)
        | (PatTuple tl, VTuple vl) ->
            let rec list_match new_env = function
                | ([],[]) -> (new_env, true)
                | ([], _) | (_, []) -> (env, false)
                | (x::xs, y::ys) ->
                    let (new_env, result) = match_with new_env (x, y) in
                    if result then
                        list_match new_env (xs, ys)
                    else
                        (env, false)
            in list_match env (tl, vl)
        | (PatTuple _, _) -> (env, false)
        | (PatCons (p1, p2), VCons (x, xs)) ->
            let (new_env, result) = match_with env (p1, x) in
            if result then
                match_with new_env (p2, xs)
            else
                (env, false)
        | (PatCons _, _) -> (env, false)
        | (PatAs (pat, id), v) ->
            let (env, result) = match_with env (pat, v) in
            if result then
                (Env.extend id (ref v) env, true)
            else
                (env, false)
        | (PatOr (p1, p2), v) ->
            let (new_env, result) = match_with env (p1, v) in
            if result then
                (new_env, true)
            else
                let (new_env, result) = match_with env (p2, v) in
                if result then
                    (new_env, true)
                else
                    (env, false)
    in
    let rec find_match env = function
        | [] -> error "match failure"
        | (pat, body)::rest ->
            let (env, result) = match_with env (pat, v) in
            if result then
                (env, body)
            else
                find_match env rest
    in
    let (env, e) = find_match env match_list in
    eval_expr env e

and eval_list env = function
    | [] -> (env, VUnit)
    | x::xs ->
        let (new_env, v) = eval_decl env x in
        if xs == [] then
            (new_env, v)
        else if v <> VUnit then
            error ("() required")
        else
            eval_list new_env xs

and eval_decl env = function
    | Let (id, e) ->
        let v = eval_expr env e in
        let new_env = Env.extend id (ref v) env in
        (new_env, VUnit)
    | LetRec (id, e) ->
        let r = ref VUnit in
        let new_env = Env.extend id r env in
        r := eval_expr new_env e;
        (new_env, VUnit)
    | Module id ->
        let new_env = Symbol.set_module id in 
        (new_env.env, VUnit)
    | Import (id, None) ->
        import id;
        (env, VUnit)
    | Import (id, Some asid) ->
        import id;
        Symbol.rename_module id asid;
        (env, VUnit)
    | TypeDef (id, typ) ->
        (env, VUnit)
    | e ->
        (env, eval_expr env e)

and import id =
    if Symbol.exist_module id then
        ()
    else begin
        let filename = make_module_filename id in
        let prev = Symbol.get_current_module () in
        let tab = Symbol.set_module id in
        (try
            load_module tab filename
        with Error s | Sys_error s -> print_endline s
            | End_of_file -> ());
        Symbol.set_current_module prev
    end

and load_module tab filename =
    try
        let text = load_file filename in
        eval_module tab @@ Parser.parse @@ Scanner.from_string text
    with
        | Error s | Sys_error s -> print_endline s
        | End_of_file -> ()

and eval_module tab el = 
    let rec loop env tenv = function
        | [] -> (env, tenv)
        | x::xs ->
            let (new_tenv, _) = Type.infer tenv x in
            let (new_env, _) = eval_decl env x in
            loop new_env new_tenv xs
    in
    let (env, tenv) = loop tab.env tab.tenv el in
    Symbol.set_current_env env;
    Symbol.set_current_tenv tenv

and load_source filename =
    try
        let text = load_file filename in
        eval_all @@ Parser.parse @@ Scanner.from_string text
    with
        | Error s | Sys_error s -> print_endline s
        | End_of_file -> ()

and eval_one e =
    let tab = Symbol.get_current_module () in
    let (tenv, t) = Type.infer tab.tenv e in
    if !g_verbose then
        print_endline (expr_to_string e)
    else ();
    let (env, v) = eval_decl tab.env e in
    Symbol.set_current_env env;
    Symbol.set_current_tenv tenv;
    (v, t)

and eval_all el = 
    let rec loop env tenv = function
        | [] -> (env, tenv)
        | x::xs ->
            let (new_tenv, _) = Type.infer tenv x in
            let (new_env, _) = eval_decl env x in
            loop new_env new_tenv xs
    in
    Symbol.set_default_module ();
    let tab = Symbol.get_current_module () in
    let (env, tenv) = loop tab.env tab.tenv el in
    Symbol.set_current_env env;
    Symbol.set_current_tenv tenv

let eval_line text =
    let e = Parser.parse_one @@ Scanner.from_string text in
    eval_one e


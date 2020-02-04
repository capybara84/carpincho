
open Syntax

let error msg = raise (Error ("Runtime error: " ^ msg))

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
        begin
            try
                !(Env.lookup id env)
            with Not_found -> error("'" ^ id ^ "' not found")
        end
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
    | Comp el ->
        let (_, v) = eval_list env el in
        v
    | _ -> failwith ("eval bug")

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
        (new_env, VUnit)
    | Import (id, None) ->
        Symbol.import_module id;
        (env, VUnit)
    | Import (id, Some ren) ->
        Symbol.import_module_as id ren;
        (env, VUnit)
    | e ->
        (env, eval_expr env e)

let eval_top top_env el = 
    Symbol.set_default_module ();
    let rec loop env = function
        | [] -> env 
        | x::xs ->
            let (new_env, v) = eval_decl env x in
            loop new_env xs
    in
    loop top_env el

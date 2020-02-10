
open Syntax

let error msg = raise (Error ("Type error: " ^ msg))

let seed = ref 0

let new_tvar () =
    let ty = TVar (!seed, ref None) in
    incr seed;
    ty

let new_type_schema () =
    let ty = new_tvar () in
    { vars = []; body = ty }

let rec equal t1 t2 =
    match (t1, t2) with
    | (TUnit, TUnit) | (TBool, TBool) | (TInt, TInt)
    | (TFloat, TFloat) | (TChar, TChar) | (TString, TString) -> true
    | (TTuple tl1, TTuple tl2) -> list_equal (tl1, tl2)
    | (TList t1, TList t2) -> equal t1 t2
    | (TFun (t11, t12), TFun (t21, t22)) ->
        equal t11 t21 && equal t12 t22
    | (TVar (n, {contents = None}), TVar (m, {contents = None}))
        -> n = m
    | (TVar (_, {contents = None}), _)
    | (_, TVar (_, {contents = None}))
        -> true
    | (TVar (_, {contents = Some t1'}), _)
        -> equal t1' t2
    | (_, TVar (_, {contents = Some t2'}))
        -> equal t1 t2'
    | _ -> false
and list_equal = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if equal x y then
            list_equal (xs, ys)
        else
            false

let rec unwrap_var free_vars = function
    | TParamId (ty, id) ->
        let (free_vars, unwrapped_type) = unwrap_var free_vars ty in
        (free_vars, TParamId (unwrapped_type, id))
    | TTuple tl ->
        let (free_vars, new_tl) = unwrap_tl_var free_vars [] tl in
        (free_vars, TTuple new_tl)
    | TList ty ->
        let (free_vars, unwrapped_type) = unwrap_var free_vars ty in
        (free_vars, TList unwrapped_type)
    | TFun (t1, t2) ->
        let (free_vars, t1') = unwrap_var free_vars t1 in
        let (free_vars, t2') = unwrap_var free_vars t2 in
        (free_vars, TFun (t1', t2'))
    | TVar (n, ({contents = None})) as typ ->
        if List.mem n free_vars then
            (free_vars, typ)
        else
            (n::free_vars, typ)
    | TVar (_, ({contents = Some t})) ->
        (free_vars, t)
    | typ -> (free_vars, typ)
and unwrap_tl_var free_vars new_tl = function
    | [] -> (free_vars, List.rev new_tl)
    | x::xs ->
        let (free_vars, t) = unwrap_var free_vars x in
        unwrap_tl_var free_vars (t::new_tl) xs


let create_poly_type ty =
    let (free_vars, unwrapped_type) = unwrap_var [] ty in
    { vars = free_vars; body = unwrapped_type }


let rec prune = function
    | TVar (_, ({contents = Some t'} as instance)) ->
        let inst = prune t' in
        instance := Some inst;
        inst
    | t -> t


let rec type_var_equal t1 t2 =
    match (t1, t2) with
    | (TVar (n, {contents = None}), TVar (m, {contents = None})) when n = m
        -> true
    | (TVar (_, {contents = Some t1'}), _)
        -> type_var_equal t1' t2
    | (_, TVar (_, {contents = Some t2'}))
        -> type_var_equal t1 t2'
    | _ -> false

let rec occurs_in_type t t2 =
    let t2 = prune t2 in
    if type_var_equal t t2 then true
    else
        match t2 with
        | TTuple tl -> occurs_in t tl
        | TList tl -> occurs_in_type t tl
        | TFun (tf1, tf2) -> occurs_in t [tf1;tf2]
        | _ -> false

and occurs_in t types =
    List.exists (fun t2 -> occurs_in_type t t2) types

let rec unify t1 t2 =
    let t1 = prune t1 in
    let t2 = prune t2 in
(*
print_endline ("T* unify " ^ type_to_string t1 ^ " " ^ type_to_string t2);
*)
    match (t1, t2) with
    | (TUnit, TUnit) | (TBool, TBool) | (TInt, TInt) | (TChar, TChar)
    | (TFloat, TFloat) | (TString, TString) -> ()
    | (TTuple tl1, TTuple tl2) ->
        (try
            List.iter2 (fun a b -> unify a b) tl1 tl2
        with Invalid_argument _ ->
            error (type_to_string t2 ^ " != " ^ type_to_string t1))
    | (TList tl, TList tr) ->
        unify tl tr
    | (TFun (t11, t12), TFun (t21, t22)) ->
        unify t11 t21;
        unify t12 t22
    | (TVar (n1, {contents = None}), TVar (n2, {contents = None}))
        when n1 = n2 -> ()
    | (TVar (_, {contents = Some t1'}), _) -> unify t1' t2
    | (_, TVar (_, {contents = Some t2'})) -> unify t1 t2'
    | (TVar (_, ({contents = None} as r1)), _) ->
        if occurs_in_type t1 t2 then
            error "circularity"
        else
            r1 := Some t2
    | (_, TVar (_, ({contents = None} as r2))) ->
        if occurs_in_type t2 t1 then
            error "circularity"
        else
            r2 := Some t1
    | (_, _) ->
        error (type_to_string t2 ^ " != " ^ type_to_string t1)

let rec infer verbose tenv e =
    if verbose then
        print_endline ("T* infer " ^ expr_to_string e)
    else ();
    match e with
    | Eof | Unit -> (tenv, TUnit)
    | Null -> (tenv, TList (new_tvar ()))
    | WildCard -> (tenv, TList (new_tvar ()))
    | BoolLit _ -> (tenv, TBool)
    | IntLit _ -> (tenv, TInt)
    | CharLit _ -> (tenv, TChar)
    (*
    | FloatLit _ -> (tenv, TFloat)
    *)
    | StrLit _ -> (tenv, TString)
    | Ident id ->
        (try
            let t = Env.lookup id tenv in
            (tenv, !t.body)
        with Not_found ->
            (try
                let t = Symbol.lookup_default_type id in
                (tenv, !t.body)
            with Not_found -> error("'" ^ id ^ "' is not defined")))
    | IdentMod (mod_name, id) ->
        (try
            let t = Symbol.lookup_type mod_name id in
            (tenv, !t.body)
        with Not_found -> error("'" ^ mod_name ^ "." ^ id ^ "' is not defined"))
    | Tuple el ->
        let tl = List.map (fun x -> let (_, t) = infer verbose tenv x in t) el in
        (tenv, TTuple tl)
    | Binary (op, lhs, rhs) ->
        let (tenv, tl) = infer verbose tenv lhs in
        let (tenv, tr) = infer verbose tenv rhs in
        let t = infer_binary op tl tr in
        (tenv, t)
    | Unary (op, e) ->
        let (tenv, t) = infer verbose tenv e in
        let t = infer_unary op t in
        (tenv, t)
    | Apply (e1, e2) ->
        let (_, t1) = infer verbose tenv e1 in
        let (_, t2) = infer verbose tenv e2 in
        let t = new_tvar () in
        unify t1 (TFun (t2, t));
        (tenv, t)
    | Fn (Ident x, e) ->
        if verbose then
            print_endline ("T* Fn (" ^ x ^ ", " ^ expr_to_string e ^ ")")
        else ();
        let t_arg = new_tvar () in
        let ts = create_poly_type t_arg in
        let tenv = Env.extend x (ref ts) tenv in
        let (tenv, t_body) = infer verbose tenv e in
        if verbose then
            print_endline ("T* after infer " ^ type_to_string t_body)
        else ();
        (tenv, TFun (t_arg, t_body))
    | Fn (WildCard, e) ->
        let t_arg = new_tvar () in
        let (tenv, t_body) = infer verbose tenv e in
        (tenv, TFun (t_arg, t_body))
    | Fn (Unit, e) ->
        let (tenv, t_body) = infer verbose tenv e in
        (tenv, TFun (TUnit, t_body))
    | Fn (_, _) -> failwith "type bug"
    | If (e1, e2, e3) ->
        let (_, t1) = infer verbose tenv e1 in
        unify TBool t1;
        let (_, t_then) = infer verbose tenv e2 in
        let (_, t_else) = infer verbose tenv e3 in
        unify t_then t_else;
        (tenv, t_then)
    | Match (e, ml) ->
        infer_match tenv e ml
    | Comp el ->
        infer_list verbose tenv el
    | Let (id, e) ->
        if verbose then
            print_endline ("T* let " ^ id ^ " = " ^ expr_to_string e)
        else ();
        let (_, t) = infer verbose tenv e in
        if verbose then
            print_endline ("T* after infer e " ^ type_to_string t)
        else ();
        let ts = create_poly_type t in
        if verbose then
            print_endline ("T* poly type = " ^ type_schema_to_string ts)
        else ();
        let tenv = Env.extend id (ref ts) tenv in
        (tenv, TUnit)
    | LetRec (id, e) ->
        let r = ref (new_type_schema ()) in
        let tenv = Env.extend id r tenv in
        let (_, t_val) = infer verbose tenv e in
        r := create_poly_type t_val;
        (tenv, TUnit)
    | TypeDef (id, typ) ->
        let ts = create_poly_type typ in
        let tenv = Env.extend id (ref ts) tenv in
        (tenv, TUnit)
    | Module id ->
        let tab = Symbol.set_module id in
        (tab.tenv, TUnit)
    | Import (_, _) ->
        (tenv, TUnit)

and infer_binary op tl tr =
(*
print_endline ("T* infer_binary " ^ string_of_binop op ^ " "
                ^ type_to_string tl ^ " " ^ type_to_string tr);
*)
    match op with
    | BinAdd ->
        unify tl tr;
(*
print_endline ("T* after unify " ^ type_to_string tl ^ " " ^ type_to_string tr);
*)
        if (equal tl TInt) || (equal tl TString) || (equal tl TFloat) then begin
(*
print_endline ("T* int or string or float");
*)
            tl
        end else
            error ("int, float or string required (" ^ type_to_string tl ^ ")")
    | BinSub | BinMul | BinDiv | BinMod ->
        unify tl tr;
        if equal tl TInt || equal tl TFloat then
            tl
        else
            error ("int or float required (" ^ type_to_string tl ^ ")")
    | BinLT | BinLE | BinGT | BinGE ->
        unify tl tr;
        if equal tl TChar || equal tl TInt || equal tl TFloat || equal tl TString then
            ()
        else
            error ("int, float, char or string required (" ^ type_to_string tl ^ ")");
        TBool

    | BinEql | BinNeq ->
        unify tl tr;
        TBool

    | BinLor | BinLand ->
        unify TBool tl;
        unify TBool tr;
        TBool

    | BinCons ->
        unify (TList tl) tr;
        (TList tl)

and infer_unary op t =
    match op with
    | UMinus ->
        unify TInt t;
        TInt
    | UNot ->
        unify TBool t;
        TBool

and infer_match tenv e ml =
    (*TODO*)
    (tenv, TUnit)

and infer_list verbose tenv el =
    match el with
    | [] -> (tenv, TUnit)
    | x::xs ->
        let (tenv, t) = infer verbose tenv x in
        if xs = [] then
            (tenv, t)
        else begin
            unify TUnit t;
            infer_list verbose tenv xs
        end


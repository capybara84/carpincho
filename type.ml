
open Syntax

let error msg = raise (Error ("Type error: " ^ msg))

let verbose msg =
    if !g_verbose then
        print_endline ("T* " ^ msg)
    else
        ()

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

let pat_equal t1 t2 =
    match (t1, t2) with
    | (TVar (_, {contents = None}), _)
    | (_, TVar (_, {contents = None})) -> true
    | _ -> equal t1 t2

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
        let (free_vars, new_t) = unwrap_var free_vars t in
        (free_vars, new_t)
    | typ -> (free_vars, typ)
and unwrap_tl_var free_vars new_tl = function
    | [] -> (free_vars, List.rev new_tl)
    | x::xs ->
        let (free_vars, t) = unwrap_var free_vars x in
        unwrap_tl_var free_vars (t::new_tl) xs


let create_poly_type ty =
    let (free_vars, unwrapped_type) = unwrap_var [] ty in
    { vars = free_vars; body = unwrapped_type }

let create_alpha_equivalent ts =
    let rec fresh_vars res_vars res_map = function
        | [] -> (List.rev res_vars, List.rev res_map)
        | x::xs ->
            let n = !seed in
            let ty = TVar (n, ref None) in
            incr seed;
            fresh_vars (n::res_vars) ((x, ty)::res_map) xs
    in
    let rec subst map = function
        | TTuple tl -> TTuple (List.map (fun x -> subst map x) tl)
        | TList t -> TList (subst map t)
        | TFun (t1, t2) -> TFun (subst map t1, subst map t2)
        | TVar (_, {contents = Some t}) -> subst map t
        | TVar (n, {contents = None}) as t->
            (try List.assoc n map with Not_found -> t)
        | t -> t
    in
    let (new_vars, var_map) = fresh_vars [] [] ts.vars in
    { vars = new_vars; body = subst var_map ts.body }


let rec prune = function
    | TVar (_, ({contents = Some t'} as instance)) ->
        let inst = prune t' in
        instance := Some inst;
        inst
    | t -> t

let rec unvar = function
    | TVar (_, {contents = Some t}) ->
        unvar t
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
(*
    let t1 = prune t1 in
    let t2 = prune t2 in
*)
    verbose ("unify " ^ type_to_string_raw t1 ^ " " ^ type_to_string_raw t2);
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
        else begin
            verbose ("unify result " ^ type_to_string_raw t1 ^ " ... ");
            r1 := Some t2;
            verbose ("... " ^ type_to_string_raw t1)
        end
    | (_, TVar (_, ({contents = None} as r2))) ->
        if occurs_in_type t2 t1 then
            error "circularity"
        else begin
            verbose ("unify result " ^ type_to_string_raw t2 ^ " ... ");
            r2 := Some t1;
            verbose ("... " ^ type_to_string_raw t2)
        end
    | (_, _) ->
        error (type_to_string t2 ^ " != " ^ type_to_string t1)

let rec infer tenv e =
    verbose ("infer " ^ expr_to_string e);
    let (tenv, ty) =
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
        verbose ("infer Ident " ^ id);
        let ts =
            (try
                Env.lookup id tenv
             with Not_found ->
                (try
                    Symbol.lookup_default_type id
                 with Not_found ->
                    error("'" ^ id ^ "' undefined" )))
        in
        verbose ("infer Ident " ^ id ^ " ... type schema is " ^ type_schema_to_string !ts);
        let new_ts = create_alpha_equivalent !ts in
        verbose ("infer Ident " ^ id ^ " ... ts alpha = " ^ type_schema_to_string new_ts);
        (tenv, new_ts.body)
    | IdentMod (mod_name, id) ->
        (try
            let ts = Symbol.lookup_type mod_name id in
            let new_ts = create_alpha_equivalent !ts in
            (tenv, new_ts.body)
        with Not_found -> error("'" ^ mod_name ^ "." ^ id ^ "' is not defined"))
    | Tuple el ->
        let tl = List.map (fun x -> let (_, t) = infer tenv x in t) el in
        (tenv, TTuple tl)
    | Binary (op, lhs, rhs) ->
        let (tenv, tl) = infer tenv lhs in
        let (tenv, tr) = infer tenv rhs in
        let t = infer_binary op tl tr in
        (tenv, t)
    | Unary (op, e) ->
        let (tenv, t) = infer tenv e in
        let t = infer_unary op t in
        (tenv, t)
    | Apply (e1, e2) ->
        let (_, t1) = infer tenv e1 in
        let (_, t2) = infer tenv e2 in
        let t = new_tvar () in
        unify t1 (TFun (t2, t));
        (tenv, t)
    | Fn (Ident x, e) ->
        verbose ("infer Fn (" ^ x ^ ", " ^ expr_to_string e ^ ")");
        let t_arg = new_tvar () in
        verbose ("infer " ^ x ^ " ... arg type = " ^ type_to_string_raw t_arg);
        let ts = { vars = []; body = t_arg } in
        verbose ("infer " ^ x ^ " ... arg ts = " ^ type_schema_to_string ts);
        let tenv = Env.extend x (ref ts) tenv in
        let (tenv, t_body) = infer tenv e in
        verbose ("after infer " ^ type_to_string_raw t_body);
        (tenv, TFun (t_arg, t_body))
    | Fn (WildCard, e) ->
        let t_arg = new_tvar () in
        let (tenv, t_body) = infer tenv e in
        (tenv, TFun (t_arg, t_body))
    | Fn (Unit, e) ->
        let (tenv, t_body) = infer tenv e in
        (tenv, TFun (TUnit, t_body))
    | Fn (_, _) -> failwith "type bug"
    | If (e1, e2, e3) ->
        verbose ("infer If " ^ expr_to_string e1 ^ " then " ^ expr_to_string e2 ^ " else "
                    ^ expr_to_string e3);
        let (_, t1) = infer tenv e1 in
        unify TBool t1;
        let (_, t_then) = infer tenv e2 in
        let (_, t_else) = infer tenv e3 in
        unify t_then t_else;
        (tenv, t_then)
    | Match (e, ml) ->
        (tenv, infer_match tenv e ml)
    | Comp el ->
        infer_list tenv el
    | Let (id, e) ->
        verbose ("infer let " ^ id ^ " = " ^ expr_to_string e);
        let (_, t) = infer tenv e in
        verbose ("after infer " ^ type_to_string_raw t);
        let ts = create_poly_type t in
        verbose ("poly type = " ^ type_schema_to_string ts);
        let tenv = Env.extend id (ref ts) tenv in
        (tenv, TUnit)
    | LetRec (id, e) ->
        verbose ("infer letrec " ^ id ^ " = " ^ expr_to_string e);
        let r = ref (new_type_schema ()) in
        verbose ("infer " ^ id ^ " type_schema = " ^ type_schema_to_string !r);
        let tenv = Env.extend id r tenv in
        let (_, t_val) = infer tenv e in
        verbose ("after infer " ^ type_to_string_raw t_val);
        r := create_poly_type t_val;
        verbose ("infer poly type = " ^ type_schema_to_string !r);
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
    in
    verbose ("infer " ^ expr_to_string e ^ " result = " ^ type_to_string_raw ty);
    (tenv, ty)

and infer_binary op tl tr =
    verbose ("infer_binary " ^ string_of_binop op ^ " "
                ^ type_to_string tl ^ " " ^ type_to_string tr);
    match op with
    | BinAdd ->
        unify tl tr;
        verbose ("after unify " ^ type_to_string tl ^ " " ^ type_to_string tr);
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

and infer_match tenv e_var pat_list =
    verbose ("infer_match " ^ expr_to_string e_var);
    let rec pattern_to_type tenv p = 
        verbose ("pattern_to_type " ^ pattern_to_string p);
        match p with
        | PatNull -> (tenv, TList (new_tvar ()))
        | PatWildCard -> (tenv, new_tvar ())
        | PatBool _ -> (tenv, TBool)
        | PatInt _ -> (tenv, TInt)
        | PatChar _ -> (tenv, TChar)
        | PatStr _ -> (tenv, TString)
        | PatIdent id ->
            let t = new_tvar () in
            let ts = { vars = []; body = t } in
            (Env.extend id (ref ts) tenv, t)
        | PatTuple pl ->
            let rec  pattern_list_to_type_list res tenv = function
                | [] -> (tenv, List.rev res)
                | x::xs ->
                    let (tenv, t) = pattern_to_type tenv x in
                    pattern_list_to_type_list (t::res) tenv xs
            in
            let (tenv, tl) = pattern_list_to_type_list [] tenv pl in
            (tenv, TTuple tl)
        | PatCons (p1, p2) ->
            let (tenv, t) = pattern_to_type tenv p1 in
            let (tenv, t') = pattern_to_type tenv p2 in
            unify t' (TList t);
            (tenv, t')
        (*
        | PatList ->
        | PatAs ->
        | PatOr ->
        *)
        | _ -> failwith "pattern_to_type bug"

    and unify_pat tenv (p, t') =
        verbose ("unify_pat (" ^ pattern_to_string p ^ ", " ^ type_to_string t' ^ ")");
        let t = unvar t' in
        match (p, t) with
        | (PatNull, TList _)
        | (PatWildCard, _)
        | (PatBool _, TBool)
        | (PatInt _, TInt)
        | (PatChar _, TChar)
        | (PatStr _, TString) -> tenv
        | (PatIdent id, t) ->
            let ts = { vars =[]; body = t } in
            Env.extend id (ref ts) tenv
        | (PatTuple pl, TTuple tl) ->
            let rec loop tenv = function
                | ([],[]) -> tenv
                | (p::ps, t::ts) ->
                    let tenv = unify_pat tenv (p, t) in
                    loop tenv (ps, ts)
                | ([],_) | (_,[]) ->
                    error ("pattern tuple error " ^ pattern_to_string p ^
                            " & " ^ type_to_string t)
            in loop tenv (pl, tl)
        | (PatList pl, t) ->
            verbose ("unify_pat (PatList (x::xs), t)");
            let check_list tenv pl =
                verbose ("check_list [" ^ pat_list_to_string pl ^ "]");
                let rec pat_list_to_type_list tenv tl = function
                    | [] -> (tenv, tl)
                    | x::xs ->
                        let (tenv, t) = pattern_to_type tenv x in
                        pat_list_to_type_list tenv (t::tl) xs
                in
                let (tenv, tl) = pat_list_to_type_list tenv [] pl in
                verbose (" type_list [" ^ type_list_to_string tl ^ "]");
                let list_same_type = function
                    | [] -> true
                    | _::[] -> true
                    | x::xs -> List.for_all (fun y -> pat_equal x y) xs
                in
                if not (list_same_type tl) then
                    error "pattern list type error"
                else ();
                (tenv, tl)
            in
            let (tenv, tl) = check_list tenv pl in
            List.iter (fun x -> unify t (TList x)) tl;
            tenv
        | (PatCons (p1, p2), TList t) ->
            verbose ("unify_pat PatCons (" ^ pattern_to_string p1 ^
                                            ", " ^ pattern_to_string p2 ^ ")");
            let tenv = unify_pat tenv (p1, t) in
            unify_pat tenv (p2, TList t)
        | (PatAs (pat, id), t) ->
            let tenv = unify_pat tenv (pat, t) in
            let ts = create_poly_type t in
            Env.extend id (ref ts) tenv
        | (PatOr (p1, p2), t) ->
            let tenv = unify_pat tenv (p1, t) in
            unify_pat tenv (p2, t)
        | (pat, TVar (_, {contents = None})) ->
            verbose ("unify_pat (pat, TVar (_, {contents = None}))");
            let (tenv, t) = pattern_to_type tenv pat in
            unify t t';
            tenv
        | _ -> error ("pattern type error (" ^ pattern_to_string p ^ " & "
                                            ^ type_to_string t ^ ")")
    in
    let (_, t_var) = infer tenv e_var in
    verbose ("after infer " ^ type_to_string t_var);
    match pat_list with
    | [] ->
        verbose "pat_list .. []";
        TUnit
    | [(pat, exp)] ->
        verbose "pat_list .. [(pat,expr)]";
        let new_tenv = unify_pat tenv (pat, t_var) in
        let (_, body_typ) = infer new_tenv exp in
        body_typ
    | (pat, exp)::rest ->
        verbose "pat_list .. (pat,expr)::rest";
        let new_tenv = unify_pat tenv (pat, t_var) in
        let (_, body_typ) = infer new_tenv exp in
        List.iter (fun (pat, exp) ->
                    let new_env = unify_pat tenv (pat, t_var) in
                    let (_, b_typ) = infer new_env exp in
                    unify body_typ b_typ) rest;
        body_typ

and infer_list tenv el =
    match el with
    | [] -> (tenv, TUnit)
    | x::xs ->
        let (tenv, t) = infer tenv x in
        if xs = [] then
            (tenv, t)
        else begin
            unify TUnit t;
            infer_list tenv xs
        end


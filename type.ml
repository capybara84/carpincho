
open Syntax

let error msg = raise (Error ("Type error: " ^ msg))

let seed = ref 0

let new_unknown () =
    let ty = TVar (!seed, ref None, ref false) in
    incr seed;
    ty

let new_tvar () =
    let ty = TVar (!seed, ref None, ref true) in
    incr seed;
    ty

let rec equal t1 t2 =
    match (t1, t2) with
    | (TUnit, TUnit) | (TBool, TBool) | (TInt, TInt)
    | (TFloat, TFloat) | (TChar, TChar) | (TString, TString) -> true
    | (TTuple tl1, TTuple tl2) -> list_equal (tl1, tl2)
    | (TList t1, TList t2) -> equal t1 t2
    | (TFun (t11, t12), TFun (t21, t22)) ->
        equal t11 t21 && equal t12 t22
    | (TVar (n, {contents = None}, _), TVar (m, {contents = None}, _))
        -> n = m
    | (TVar (_, {contents = None}, _), _)
    | (_, TVar (_, {contents = None}, _))
        -> true
    | (TVar (_, {contents = Some t1'}, _), _)
        -> equal t1' t2
    | (_, TVar (_, {contents = Some t2'}, _))
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



let rec prune = function
    | TVar (_, ({contents = Some t'} as instance), _) ->
        let inst = prune t' in
        instance := Some inst;
        inst
    | t -> t


let rec type_var_equal t1 t2 =
    match (t1, t2) with
    | (TVar (n, {contents = None}, _), TVar (m, {contents = None}, _)) when n = m
        -> true
    | (TVar (_, {contents = Some t1'}, _), _)
        -> type_var_equal t1' t2
    | (_, TVar (_, {contents = Some t2'}, _))
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

let is_generic v non_generic =
    not (occurs_in v non_generic)

let rec unify t1 t2 =
    let t1 = prune t1 in
    let t2 = prune t2 in
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
    | (TVar (n1, {contents = None}, _), TVar (n2, {contents = None}, _))
        when n1 = n2 -> ()
    | (TVar (_, {contents = Some t1'}, _), _) -> unify t1' t2
    | (_, TVar (_, {contents = Some t2'}, _)) -> unify t1 t2'
    | (TVar (_, ({contents = None} as r1), _), _) ->
        if occurs_in_type t1 t2 then
            error "circularity"
        else
            r1 := Some t2
    | (_, TVar (_, ({contents = None} as r2), _)) ->
        if occurs_in_type t2 t1 then
            error "circularity"
        else
            r2 := Some t1
    | (_, _) ->
        error (type_to_string t2 ^ " != " ^ type_to_string t1)

let rec infer tenv e =
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
            (tenv, t)
        with Not_found ->
            (try
                let t = Symbol.lookup_default_type id in
                (tenv, t)
            with Not_found -> error("'" ^ id ^ "' is not defined")))
    | IdentMod (mod_name, id) ->
        (try
            let t = Symbol.lookup_type mod_name id in
            (tenv, t)
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
        let t_arg = new_tvar () in
        let tenv = Env.extend x t_arg tenv in
        let (tenv, t_body) = infer tenv e in
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
        let (_, t1) = infer tenv e1 in
        unify TBool t1;
        let (_, t_then) = infer tenv e2 in
        let (_, t_else) = infer tenv e3 in
        unify t_then t_else;
        (tenv, t_then)
    | Match (e, ml) ->
        infer_match tenv e ml
    | Comp el ->
        infer_list tenv el
    | Let ((id, ty), e) ->
        let (_, t) = infer tenv e in
        unify ty t;
        let tenv = Env.extend id ty tenv in
        (tenv, TUnit)
    | LetRec ((id, ty), e) ->
        let tenv = Env.extend id ty tenv in
        let (_, t_val) = infer tenv e in
        unify t_val ty;
        (tenv, TUnit)
    | TypeDef (id, typ) ->
        let tenv = Env.extend id typ tenv in
        (tenv, TUnit)
    | Module id ->
        let tab = Symbol.set_module id in
        (tab.tenv, TUnit)
    | Import (_, _) ->
        (tenv, TUnit)

and infer_binary op tl tr =
    match op with
    | BinAdd ->
        unify tl tr;
        if equal tl TInt || equal tl TString || equal tl TFloat then
            tl
        else
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


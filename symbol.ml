open Syntax

let default_module_name = "Main"

let all_modules = ref Env.empty

let get_all_modules () = !all_modules

let lookup_module id =
    Env.lookup id !all_modules

let exist_module id =
    try ignore (Env.lookup id !all_modules);
        true
    with Not_found -> false

let insert_module id =
    let tab = { env = Env.empty; tenv = Env.empty } in
    all_modules := Env.extend id tab !all_modules;
    tab

let default_module = insert_module default_module_name
let current_module = ref default_module

let get_current_module () = !current_module
let set_current_module tab =
    current_module := tab

let set_default_module () =
    current_module := default_module

let get_current_module () = !current_module
let set_current_env env = !current_module.env <- env
let set_current_tenv tenv = !current_module.tenv <- tenv

let set_module id =
    (try
        current_module := lookup_module id
    with Not_found ->
        current_module := insert_module id);
    !current_module

let insert_default name ty fn =
    default_module.env <- Env.extend name (ref fn) default_module.env;
    default_module.tenv <- Env.extend name ty default_module.tenv

let lookup_default id =
    Env.lookup id (default_module.env)

let lookup mod_name id =
    let symtab = lookup_module mod_name in
    Env.lookup id symtab.env

let lookup_default_type id =
    Env.lookup id (default_module.tenv)

let lookup_type mod_name id =
    let symtab = lookup_module mod_name in
    Env.lookup id symtab.tenv

let rename_module old_name new_name =
    let tab = lookup_module old_name in
    let module_list = List.remove_assoc old_name !all_modules in
    all_modules := Env.extend new_name tab module_list


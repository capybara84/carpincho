open Syntax

let default_module_name = "Main"

let all_modules = ref Env.empty

let get_all_modules () = !all_modules

let lookup_module id =
    Env.lookup id !all_modules

let insert_module id =
    let modu = { name = id; env = Env.empty } in
    all_modules := Env.extend id modu !all_modules;
    modu

let default_module = insert_module default_module_name
let current_module = ref default_module


let get_default_env () = default_module.env
let set_default_env env = default_module.env <- env

let get_current_module () = !current_module
let set_current_module modu =
    current_module := modu

let set_default_module () =
    current_module := default_module

let get_current_env () = !current_module.env
let set_current_env env = !current_module.env <- env


let set_module id =
    (try
        current_module := lookup_module id
    with Not_found ->
        current_module := insert_module id);
    !current_module.env

let lookup_default id =
    Env.lookup id (default_module.env)

let lookup mod_name id =
    let symtab = lookup_module mod_name in
    Env.lookup id symtab.env


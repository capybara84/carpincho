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

let set_default_module () =
    current_module := default_module

let set_module id =
    (try
        current_module := lookup_module id
    with Not_found ->
        current_module := insert_module id);
    !current_module.env

let import_module id =
    (*TODO*)
    ()

let import_module_as id ren =
    (*TODO*)
    ()

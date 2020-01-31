
let color c s = "\x1b[3" ^ (string_of_int c) ^ "m" ^ s ^ "\x1b[0m"
let red = color 1
let green = color 2
let yellow = color 3
let blue = color 4
let magenta = color 5
let cyan = color 6
let white = color 7

let n_ok = ref 0
let n_fail = ref 0

let ok () =
    incr n_ok;
    print_string @@ green "."

let fail s =
    incr n_fail;
    print_endline @@ red "!" ^ s

let equal a b m =
    if a = b then
        ok ()
    else
        fail m

let report () =
    let n_all = !n_ok + !n_fail in
    print_endline ("All   : " ^ (string_of_int n_all));
    print_endline ("OK    : " ^ (green @@ string_of_int !n_ok));
    print_endline ("Failed: " ^ (if !n_fail = 0 then "0" else (red @@ string_of_int !n_fail)))

let test_list = ref []

let add name verbose (fn : bool -> unit) =
    test_list := (name, verbose, fn) :: !test_list

let run () =
    let do_test (name, verbose, fn) =
        print_string (name ^ " ");
        fn verbose;
        print_newline ()
    in
    print_endline "Test";
    List.iter do_test (List.rev !test_list);
    report ()

(*
let () =
    add "test" true (fun verbose -> ok();ok();fail "x"; fail "y");
    run ()
*)


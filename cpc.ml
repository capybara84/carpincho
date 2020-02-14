
open Syntax

let filenames = ref []

let welcome () =
    print_endline "cpc - carpincho v0.0"

let rec top_level () =
    try
        print_string "> ";
        flush stdout;
        let line = input_line stdin in
        let (v, t) = Eval.eval_line line in
        if v <> VUnit then begin
            print_endline @@ value_to_string v ^ " : " ^ type_to_string t
        end else
            ();
        top_level ()
    with
        | Error s -> (print_endline s; top_level ())
        | Sys_error s -> print_endline s
        | End_of_file -> ()


let do_test () =
    Test_scanner.init !g_verbose;
    Test_parser.init !g_verbose;
    Test_eval.init !g_verbose;
    Test_type.init !g_verbose;
    Test.run()

let main () =
    let interactive = ref false in
    let test = ref false in
    Builtins.init ();

    Arg.parse [("-d", Arg.Int (fun n -> Parser.debug := n),
                        "N  set debug level N");
               ("-v", Arg.Unit (fun () -> g_verbose := true), "  verbose");
               ("-t", Arg.Unit (fun () -> test := true), "  test mode");
               ("-i", Arg.Unit (fun () -> interactive := true),
                                "  interactive mode"); ]
        (fun name -> filenames := name::!filenames)
        "usage: cpc [-d N][-v][-t][-i] filename...";
    List.iter Eval.load_source (List.rev !filenames);

    if !test then
        do_test ()
    else if !interactive then
        top_level ()
    else ()

let () = main ()


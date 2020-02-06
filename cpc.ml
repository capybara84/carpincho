
let filenames = ref []

let welcome () =
    print_endline "cpc - carpincho v0.0"

let rec top_level () =
    try
        print_string "> ";
        flush stdout;
        let line = input_line stdin in
        let v = Eval.eval_line line in
        if v <> Syntax.VUnit then
            print_endline @@ Syntax.value_to_string v
        else
            ();
        top_level ()
    with
        | Syntax.Error s -> (print_endline s; top_level ())
        | Sys_error s -> print_endline s
        | End_of_file -> ()


let do_test () =
    Test_scanner.init false;
    Test_parser.init false;
    Test_eval.init false;
    Test_type.init false;
    Test.run()

let main () =
    let interactive = ref false in
    Builtins.init ();

    Arg.parse [("-d", Arg.Int (fun n -> Parser.debug := n),
                        "N  set debug level N");
               ("-t", Arg.Unit do_test, "  test mode");
               ("-i", Arg.Unit (fun () -> interactive := true),
                                "  interactive mode"); ]
        (fun name -> filenames := name::!filenames)
        "usage: cpc [-ti][-d N] filename...";
    List.iter Eval.load_source (List.rev !filenames);

    if !interactive then
        top_level()
    else ()

let () = main ()


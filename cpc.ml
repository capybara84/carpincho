
let filenames = ref []

let welcome () =
    print_endline "cpc - carpincho v0.0"

let top_level () =
    let rec loop () =
        try
            print_string "> ";
            flush stdout;
            let line = input_line stdin in
            let v = Eval.eval_one
                        @@ Parser.parse_one @@ Scanner.from_string line
            in
            if v <> Syntax.VUnit then
                print_endline @@ Syntax.value_to_string v
            else
                ();
            loop ()
        with
            | Syntax.Error s -> (print_endline s; loop ())
            | Sys_error s -> print_endline s
            | End_of_file -> ()
    in loop ()

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let load_source filename =
    try
        let text = load_file filename in
        Eval.eval_all @@ Parser.parse @@ Scanner.from_string text
    with
        | Syntax.Error s -> print_endline s
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let do_test () =
    Test_scanner.init false;
    Test_parser.init false;
    Test_eval.init false;
    Test.run()

let main () =
    let interactive = ref false in

    let env = Symbol.get_default_env () in
    let env = Builtins.init env in
    Symbol.set_default_env env;

    Arg.parse [("-d", Arg.Int (fun n -> Parser.debug := n),
                        "N  set debug level N");
               ("-t", Arg.Unit do_test, "  test mode");
               ("-i", Arg.Unit (fun () -> interactive := true),
                                "  interactive mode"); ]
        (fun name -> filenames := name::!filenames)
        "usage: cpc [-ti][-d N] filename...";
    List.iter load_source (List.rev !filenames);
    if !interactive then
        top_level()
    else ()

let () = main ()


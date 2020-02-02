
let filenames = ref []

let welcome () =
    print_endline "cpc - carpincho v0.0"

let top_level () =
    let rec loop env =
        try
            print_string "> ";
            flush stdout;
            let line = input_line stdin in
            let (new_env, v) =
                Eval.eval_decl env @@ Parser.parse_one @@ Scanner.from_string line
            in
            print_endline @@ Syntax.value_to_string v;
            loop new_env
        with
            | Syntax.Error s -> (print_endline s; loop env)
            | Sys_error s -> print_endline s
            | End_of_file -> ()
    in loop []

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let load_source filename =
    try
        let text = load_file filename in
        Eval.eval_top @@ Parser.parse @@ Scanner.from_string text
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
    Arg.parse [("-t", Arg.Unit do_test, "test mode");
               ("-i", Arg.Unit top_level, "interactive mode"); ]
        (fun n -> filenames := n :: !filenames)
        "usage: cpc [-ti] filename...";
    List.iter load_source (List.rev !filenames)

let () = main ()

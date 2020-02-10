
open Syntax

let filenames = ref []

let welcome () =
    print_endline "cpc - carpincho v0.0"

let rec top_level verbose =
    try
        print_string "> ";
        flush stdout;
        let line = input_line stdin in
        let (v, t) = Eval.eval_line verbose line in
        if v <> VUnit then begin
            print_endline @@ value_to_string v ^ " : " ^ type_to_string t
        end else
            ();
        top_level verbose
    with
        | Error s -> (print_endline s; top_level verbose)
        | Sys_error s -> print_endline s
        | End_of_file -> ()


let do_test verbose =
    Test_scanner.init verbose;
    Test_parser.init verbose;
    Test_eval.init verbose;
    Test_type.init verbose;
    Test.run()

let main () =
    let interactive = ref false in
    let test = ref false in
    let verbose = ref false in
    Builtins.init ();

    Arg.parse [("-d", Arg.Int (fun n -> Parser.debug := n),
                        "N  set debug level N");
               ("-v", Arg.Unit (fun () -> verbose := true), "  verbose");
               ("-t", Arg.Unit (fun () -> test := true), "  test mode");
               ("-i", Arg.Unit (fun () -> interactive := true),
                                "  interactive mode"); ]
        (fun name -> filenames := name::!filenames)
        "usage: cpc [-d N][-v][-t][-i] filename...";
    List.iter (Eval.load_source !verbose) (List.rev !filenames);

    if !test then
        do_test !verbose
    else if !interactive then
        top_level !verbose
    else ()

let () = main ()


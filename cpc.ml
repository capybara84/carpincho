
let welcome () =
    print_endline "cpc - carpincho v0.0"

let main () =
    Test_scanner.init false;
    Test_parser.init false;
    Test_eval.init false;
    Test.run()

let () = main ()

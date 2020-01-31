
let welcome () =
    print_endline "cpc - carpincho v0.0"

let main () =
    welcome ();
    Test_scanner.init true;
    Test.run()

let () = main ()

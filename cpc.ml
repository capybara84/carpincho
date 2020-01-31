
let welcome () =
    print_endline "cpc - carpincho v0.0"

let main () =
    welcome ();
    Test_scanner.init false;
    Test.run()

let () = main ()

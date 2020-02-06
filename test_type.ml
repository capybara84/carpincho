
open Syntax

let type_print_test _ =
    print_newline();
    print_endline @@ type_to_string (TTuple [TInt;TInt]);
    print_endline @@ type_to_string (TTuple [TInt;TInt;TInt]);
    print_endline @@ type_to_string (TTuple [TTuple [TInt;TInt];TInt]);
    print_endline @@ type_to_string (TTuple [TInt;TTuple [TInt;TInt]]);
    print_endline @@ type_to_string (TTuple [TTuple [TInt;TInt];TTuple [TInt;TInt]]);
    print_endline @@ type_to_string (TFun (TInt,TInt));
    print_endline @@ type_to_string (TFun (TInt, TFun (TInt, TInt)));
    print_endline @@ type_to_string (TFun (TFun (TInt, TInt), TInt));
    print_endline @@ type_to_string (TFun (TInt, TTuple [TInt; TInt]));
    print_endline @@ type_to_string (TFun (TTuple [TInt; TInt], TInt));
    print_endline @@ type_to_string (TList TInt);
    print_endline @@ type_to_string (TList (TTuple [TInt;TInt;TInt]));
    print_endline @@ type_to_string (TList (TFun (TInt,TInt)));
    print_endline @@ type_to_string (TList (TFun (TFun (TInt,TInt),TInt)));
    ()

let init verbose =
    Test.add "Type Print" verbose type_print_test

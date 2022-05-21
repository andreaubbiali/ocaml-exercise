let e1q3 number sep = 
    let rec exec res num = match (num/10) with
        0 when number == 0           -> ""
    |   0                            -> ((string_of_int (num mod 10))^sep^res)
    |   n                            -> exec ((string_of_int (num mod 10))^sep^res) (n)
    in exec "" number;;

let main() = 
    Printf.printf "EXECUTING:\n";
    Printf.printf "Expected '' got: %s\n" (e1q3 0 ":");
    Printf.printf "Expected '7:' got: %s\n" (e1q3 7 ":");
    Printf.printf "Expected '7:0:' got: %s\n" (e1q3 70 ":");
    Printf.printf "Expected '7:2:' got: %s\n" (e1q3 72 ":");
    Printf.printf "Expected '9:8:7:' got: %s\n" (e1q3 987 ":");
    Printf.printf "Expected '9:8:7:6:' got: %s\n" (e1q3 9876 ":");;

main();;
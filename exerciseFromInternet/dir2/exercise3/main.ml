
let e1q2 number sep = 
    let rec exec res num = match (num/10) with
        0 when number == 0           -> ""
    |   0                            -> (res^(string_of_int (num mod 10))^sep)
    |   n                            -> exec (res^(string_of_int (num mod 10))^sep) (n)
    in exec "" number;;

let main() = 
    Printf.printf "EXECUTING:\n";
    Printf.printf "Expected '' got: %s\n" (e1q2 0 ":");
    Printf.printf "Expected '7:' got: %s\n" (e1q2 7 ":");
    Printf.printf "Expected '0:7:' got: %s\n" (e1q2 70 ":");
    Printf.printf "Expected '2:7:' got: %s\n" (e1q2 72 ":");
    Printf.printf "Expected '7:8:9:' got: %s\n" (e1q2 987 ":");
    Printf.printf "Expected '6:7:8:9:' got: %s\n" (e1q2 9876 ":");;

main();;
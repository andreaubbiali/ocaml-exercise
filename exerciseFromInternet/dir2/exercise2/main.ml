
let e1q4 number =
        let rec execute res n = match (Int64.div n 10L) with
            0L when Int64.equal number 0L               -> res
        |   0L                                          -> (Int64.add res 1L)    
        |   num                                         -> execute (Int64.add res 1L) num
        in execute 0L number;;



let main() = 
    Printf.printf "EXECUTING:\n";
    Printf.printf "Expected 0 got: %Lu\n" (e1q4 0L);
    Printf.printf "Expected 1 got: %Lu\n" (e1q4 1L);
    Printf.printf "Expected 2 got: %Lu\n" (e1q4 10L);
    Printf.printf "Expected 3 got: %Lu\n" (e1q4 999L);;



main();;
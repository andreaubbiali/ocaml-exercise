
let e1q1 rep str = 
    let rec exec res = function
        0   -> String.concat "" res
    |   n   -> exec (res@[str]) (n-1)
    in exec [] rep;;
    

let main() =
            Printf.printf "Expected '' got %s\n" (e1q1 0 "ab");
            Printf.printf "Expected 'ab' got %s\n" (e1q1 1 "ab");
            Printf.printf "Expected 'abab' got %s\n" (e1q1 2 "ab");
            Printf.printf "Expected 'abababab' got %s\n" (e1q1 4 "ab");;

main();;
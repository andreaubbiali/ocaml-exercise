module StackList = struct

    type primitive = string;;
    type t = {
        mutable c: primitive list
    }
    
    exception EmtpyException;;

    let empty = {c = []};;
    
    let add v lst = lst.c <- [v]@lst.c;;

    let removeFirstEl lst = match lst.c with
        []      -> raise EmtpyException
    |   h::tl   -> lst.c <- tl; h;;

    let isEmpty lst = (List.length lst.c) == 0;;
    
    let printa lst = 
        let rec exec = function
            []      -> ()
        |   h::tl   -> Printf.printf "%s\n" (h); exec tl
        in exec lst.c;;
end;;
(* Pack consecutive duplicates of list elements into sublists.  *)

(* ok ma da fare il print della lista *)

let pack lst = 
    let rec execute tmp res = function
        []          -> res
    |   h::t        -> if (List.hd tmp) = h then
                            execute (tmp@[h]) res t
                        else
                            execute [h] (res@[tmp]) t
    in execute [List.hd lst] [] lst;;


let main() = 
    let lst = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
    in
    Printf.printf "BEFORE: ";
    printa_one_list(lst);
    Printf.printf "AFTER: ";
    printa(pack(lst));;

main()
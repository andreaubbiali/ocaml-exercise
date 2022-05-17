(* Eliminate consecutive duplicates of list elements.*)

let rec printa = function
    []        -> Printf.printf "Nothing in the list\n"  
|    h::[]    -> Printf.printf "%s \n" (h)
|    h::t     -> Printf.printf "%s-" (h); printa t;;


(* versione  internet*)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

(* MIA VERSIONE
let compress lst = 
    let rec comp res old = function
        []      -> res
    |   h::t    -> if (String.equal h old) then 
                        comp res old t
                    else
                        comp (res@[h]) h t
    in comp [List.hd lst] (List.hd lst) (List.tl lst);;
*)

let main() = 
    let lst = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    in
    Printf.printf "BEFORE: ";
    printa(lst);
    Printf.printf "AFTER: ";
    printa(compress(lst));;

main();;
(* 
It is known that a welfare crook lives in Los Angeles. You are given lists
for 1) people receiving welfare, 2) Hollywood actors, and 3) residents of Beverly Hills.
The names in each list are sorted alphabetically (by < ). A welfare crook is someone
who appears in all three lists. Write an algorithm to find at least one crook
*)

(* liste già ordinate  deve uscire Laura*)
let ls1 = ["Andrea"; "Laura"; "zur"];;
let ls2 = ["Francois"; "Laura"; "Lauren"; "zur"];;
let ls3 = ["Dieg"; "Ramon"; "zur"];;

let rec is_in name = function
    []      -> false
  | h::tl   -> if name = h then true else is_in name tl;;

let rec ricerca l2 l3 = function
    []      -> Printf.printf "NON ESISTE, ERRORE PERCHÈ DOVREBBE esistere\n"
  | h::tl   -> if is_in h l2 
                then 
                  if is_in h l3 then
                    Printf.printf "%s\n" h
                  else ricerca l2 l3 tl
                else ricerca l2 l3 tl;;

ricerca ls2 ls1 ls3;;

ricerca ls1 ls2 ls3;;

  


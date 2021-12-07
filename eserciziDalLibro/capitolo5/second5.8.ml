(* 
It is known that a welfare crook lives in Los Angeles. You are given lists
for 1) people receiving welfare, 2) Hollywood actors, and 3) residents of Beverly Hills.
The names in each list are sorted alphabetically (by < ). A welfare crook is someone
who appears in all three lists. Write an algorithm to find at least one crook
*)

(* liste già ordinate  deve uscire Zur*)
let ls1 = ["Andrea"; "Laura"; "zur"];;
let ls2 = ["Francois"; "Laura"; "Lauren"; "zur"];;
let ls3 = ["Dieg"; "Ramon"; "zur"];;

(* return true if the element is in the list, false otherwise *)
let is_element_in_list l a = 
  let rec is_in = function
     h::t   -> if h = a then true else is_in t
    | []    -> false
  in is_in l;;

(* return one person in all the three list *)
let find = 
  let rec itera = function
     h::t     -> if (is_element_in_list ls2 h) && (is_element_in_list ls3 h) then
                      h
                else
                      itera t
    | []      -> raise(Invalid_argument "No one is in all lists")
  in itera ls1;;


let main() = Printf.printf "%s\n" find;;

main();;
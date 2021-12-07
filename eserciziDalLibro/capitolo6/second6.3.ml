(* 
Suppose we have the following definition for a type of small numbers.
    type small = Four | Three | Two | One

The builtin comparison (<) orders the numbers in reverse order.
    #Four < Three;;
    - : bool = true

  1.  Write a function lt_small : small -> small -> bool that orders the numbers in the normal way.

  2.  Suppose the type small defines n small integers. How does the size of your code depend on n?
*)

(* sono ordinati al contrario *)
type small = 
    Four 
  | Three 
  | Two 
  | One;;

let small_to_int = function
| Four -> 4 | Three     -> 3 | Two -> 2 | One -> 1
;;

let lt_small a b = (small_to_int a) < (small_to_int b);;


let main() = Printf.printf "Prima: four < three %b \n" (Four<Three);
             Printf.printf "Dopo: four < three %b \n"  (lt_small Four Three);;

main();;
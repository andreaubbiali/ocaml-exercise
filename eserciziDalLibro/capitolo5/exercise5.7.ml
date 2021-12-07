(* 
The function append : ’a list -> ’a list -> ’a list appends two
lists. It can be defined as follows.

    let rec append l1 l2 =
      match l1 with
      h :: t -> h :: append t l2
      | [] -> l2

Write a tail-recursive version of append .
*)

let ls1 = [5; 6];;
let ls2 = [4; 3];;

(* non tail recursive *)
let rec append l1 l2 =
  match l1 with
  h :: t -> h :: append t l2
  | [] -> l2

(* tail recursive *)
let append2 l1 l2 = 
  let rec app2 lst = function
      []      -> lst
    | h::tl   -> app2 (h::lst) tl
  in app2 l2 l1;;


let main() = List.iter (Printf.printf " %d ") (append ls1 ls2);
             Printf.printf "\n";
             List.iter (Printf.printf " %d ") (append2 ls1 ls2);
             Printf.printf "\n";;

main();;
(* 
Suppose you are implementing a relational employee database, where
the database is a list of tuples name * phone * salary .

  1. Write a function find_salary : string -> float that returns the salary of an
    employee, given the name.
  
  2. Write a general function select : (string * string * float -> bool) -> (string * string * float) list
    that returns a list of all the tuples that match the predicate. For example the ex-
    pression select (fun (_, _, salary) -> salary < 100.0) would return the
    tuples for John and Joan.

*)

let db =
  ["John", "x3456", 50.1;
  "Jane", "x1234", 107.3;
  "Joan", "unlisted", 12.7];;

let fst (x, _, _) = x;;
let snd (_, x, _) = x;;
let trd (_, _, x) = x;;

let printa a = Printf.printf "   %s     %s     %f\n" (fst(a)) (snd(a)) (trd(a));;


let find_salary name =
  let rec ricors = function
    []      -> raise (Invalid_argument "nth2")
  | h::th   -> if name = fst(h) then trd(h) else ricors th
  in ricors db;;

let select funct = 
  let rec ricors = function
    []      -> Printf.printf "fine\n"
  | h::th   -> if funct h then printa h; ricors th
  in ricors db;;


let main() = Printf.printf "expected 50.1:  %f\n" (find_salary "John");
             Printf.printf "expected 107.3:  %f\n" (find_salary "Jane");
             select (fun (_, _, salary) -> salary < 100.0);;

main();;
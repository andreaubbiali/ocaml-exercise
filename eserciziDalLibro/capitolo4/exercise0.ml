(* prova di un pattern matching *)

let rec fib i =
  match i with
    0 -> 0
  | 1 -> 1
  | j -> fib(j-2)+fib(j-1);;

let main() = Printf.printf "%d\n" (fib 3);;

main();;
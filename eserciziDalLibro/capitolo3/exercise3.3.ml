(* Write a function sum that, given two integer bounds n and m and a function
f , computes a summation 
sommatoria(da i=n fino a che i=m) f(i) *)

let sum num1 num2 f =
  let rec sum2 somma i =
    if i = num2 then somma else sum2 (somma + f i) (i+1)
  in
  sum2 0 num1;;

let main() =  Printf.printf "%d\n" (sum 1 3 (fun x -> x+1)); (*=5*)
              Printf.printf "%d\n" (sum 1 5 (fun x -> x+1));; (*=14*)

main();;
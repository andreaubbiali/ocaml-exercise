(* 
A type of unary (base-1) natural numbers can be defined as follows,
    type unary_number = Z | S of unary_number

where Z represents the number zero, and if i is a unary number, then S i is i + 1. For
example, the number 5 would be represented as S (S (S (S (S Z)))) .

  1.  Write a function to add two unary numbers. What is the complexity of your function?

  2.  Write a function to multiply two unary numbers.
*)

type unary_number = 
    Z 
  | S of unary_number;;

let numero5 = S (S (S (S (S Z))));;
let numero8 = S (S (S (S (S (S (S (S Z)))))));;

let rec calcola = function
    Z          -> 0
  | S num      -> 1 + calcola num;;

let numero = Printf.printf "expected 5 have:   %d\n" (calcola numero5);;
let numero = Printf.printf "expected 8 have:   %d\n" (calcola numero8);;

let (+) a b = calcola(a) + calcola(b);;

let numero = Printf.printf "expected 13 have:   %d\n" (numero5+numero8);;

let ( * ) a b = calcola(a) * calcola(b);;

let numero = Printf.printf "expected 40 have:   %d\n" (numero5*numero8);;
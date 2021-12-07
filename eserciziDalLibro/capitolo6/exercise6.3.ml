(* 
Suppose we have the following definition for a type of small numbers.
    type small = Four | Three | Two | One

The builtin comparison (<) orders the numbers in reverse order.
    #Four < Three;;
    - : bool = true

  1.  Write a function lt_small : small -> small -> bool that orders the numbers in the normal way.

  2.  Suppose the type small defines n small integers. How does the size of your code depend on n?
*)

type small = 
    Four 
  | Three 
  | Two 
  | One;;
(* sono al contrario *)
let numero = Printf.printf "(Four < Three) expected true have:   %b \n" (Four < Three);;
let numero = Printf.printf "(Four < two)  expected true have:   %b \n" (Four < Two);;

(* richiesta è lt_small a b = è b<a?
let lt_small a = function
    Four      -> false
  | Three     -> match a with 
                     Four    -> true
                  |  Three   -> false
                  |  Two     -> false
                  |  One     -> false
  | Two     -> match a with 
                     Four    -> true
                  |  Three   -> true
                  |  Two     -> false
                  |  One     -> false 
  | One     -> match a with 
                     Four    -> true
                  |  Three   -> true
                  |  Two     -> true
                  |  One     -> false
  | _       -> raise(Invalid_argument "error in function lt_small");;
                  

let numero = Printf.printf "(Four < Three) con lt_small expected false have:   %b \n" (lt_small Three Four);;
let numero = Printf.printf "(Three < Four) con lt_small expected true have:   %b \n" (lt_small Four Three);;
let numero = Printf.printf "(Two < Four) con lt_small expected true have:   %b \n" (lt_small Four Two);;
let numero = Printf.printf "(One < Four) con lt_small expected true have:   %b \n" (lt_small Four One);; *)

  (*
  risposta 2 
      type small = 
          Four 
        | Three 
        | Two 
        | One;;
    Il mio codice non dipende da n. Basta scrivere in ordine il type small e la funzione (<) funziona bene

  *)

type smallOrdered = 
    One
  | Two
  | Three
  | Four

let numero = Printf.printf "(Four < Three) expected false have:   %b \n" (Four < Three);;
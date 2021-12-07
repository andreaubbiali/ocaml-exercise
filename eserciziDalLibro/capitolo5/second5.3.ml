(* 
One of the issues with tuples is that there is no general destructor function
that takes a tuple and projects an element of it. Suppose we try to write one for triples.
let nth i (x, y, z) =
  match i with
  1 -> x
  | 2 -> y
  | 3 -> z
  | _ -> raise (Invalid_argument "nth")

1. What is the type of the nth function? 
2. Is there a way to rewrite the function so that it allows the elements of the tuple
    to have different types?
*)

let fst (x, y, z) = x;;
let snd (x, y, z) = y;;
let trd (x, y, z) = z;;

(* function with only one return type *)
let nth i (x, y, z) =
  match i with
  1 -> x
  | 2 -> y
  | 3 -> z
  | _ -> raise (Invalid_argument "nth");;

type ('a,'b,'c) t =
  | Fst of 'a
  | Snd of 'b
  | Thd of 'c

let nth2 i (x,y,z) =
  match i with
  1 -> Fst x
  | 2 -> Snd y
  | 3 -> Thd z
  | _ -> raise (Invalid_argument "nth");;

let main ()= Printf.printf "%d\n" (nth 1 (5, 6, 7));
             Printf.printf "%s\n" (nth 1 ("aa", "bb", "cc"));;
             (* Printf.printf "%d\n" (nth2 1 ("aa", 2, "cc"));; *)


main();;

(* 
  È un esercizio strano perchè una funzione non può tornare dei tipi diversi. Per come l'ho impostata torna si dei tipi diversi ma 
  essendo Ocaml staticamente tipato come faccio ad utilizzarlo per altre funzioni? Per esempio se tolgo commento riga 42 della print
  mi da errore perchè non sa che tipo di valore gli ritorna e non sa come lavorarlo.
*)
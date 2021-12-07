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

let fst (x, _, _) = x;;
let snd (_, x, _) = x;;
let trd (_, _, x) = x;;

let tripla x y z = (x, y, z);;

let nth i (x, y, z) =
  match i with
  1 -> x
  | 2 -> y
  | 3 -> z
  | _ -> raise (Invalid_argument "nth");;

let nth2 i tripl =
  match i with
  1 -> fst(tripl)
  | 2 -> snd(tripl)
  | 3 -> trd(tripl)
  | _ -> raise (Invalid_argument "nth2");;

  (* NON CREDO SIA GIUSTO, sono riuscito a fare risultare i giusti tipi ma è sbagliato*)


(* let main ()= Printf.printf "%d\n" (nth 1 (5, 6, 7));
             Printf.printf "%s\n" (nth 1 ("aa", "bb", "cc"));; *)
             (* Printf.printf "%s\n" (nth2 1 ("aa", "bb", "cc"));; *)


(* main();; *)
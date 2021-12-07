(* 
Esercizio su interfacce pensato da me perchè sul libro non sono belli

creare un type set con funzioni 
empty
add
choose


ocamlc -c exerciseMio.mli
ocamlc -c exerciseMio.ml
*)

type 'a set = 'a list;;

type 'a choice = 
    Element of 'a
  | Empty;;

let empty = [];;

let add newElement l = newElement :: l;;

let rec choose x = function
    h::tl   -> if x == h then true else choose x tl
  | []      -> false;;


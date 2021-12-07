(* 
Suppose you are given the following definition of a list type.
  type ’a mylist = Nil | Cons of ’a * ’a mylist

    1.  Write a function map : (’a -> ’b) -> ’a mylist -> ’b mylist , where
      map f [ x 0 ; x 1 ; · · · ; x n ] = [ f x 0 ; f x 1 ; · · · ; f x n ] .

    2.  Write a function append : ’a mylist -> ’a mylist -> ’a mylist , where
      append [ x 1 ;· · · ; x n ] [ x n+1 ; · · · ; x n+m ] = [ x 1 ; · · · ; x n+m ] .

*)

(* lista con un 'a ed un puntatore ad un 'a mylist *)
type 'a mylist = 
    Nil 
  | Cons of 'a * 'a mylist;;

let numero = Cons (6, Nil);;
let stringa = Cons ("sjd", Cons ("sd", Nil));;


let rec map3 func mylst= 
  match mylst with
    Cons (a, b) -> Cons (func(a), map3 func b)
  | Nil         -> Nil;;

(* parzialmente giusto.. non ’a mylist -> ’a mylist -> ’a mylist ma ’a mylist -> ’a mylist -> ’b mylist *)
let rec append mylst1 mylst2 =
  match mylst2 with
    Cons (a,b) -> append (Cons(a, mylst1)) b
  | Nil        -> Nil;;

let rec append2 mylst1 = function
    Cons (a,b) -> append2 (Cons(a, mylst1)) b
  | Nil        -> Nil;;
(* esercizio del profe *)

(* let is_balanced str =
  let s = Stack.empty in try
    String.iter
    (fun c -> match c with
        '(' -> Stack.push s c
      | ')' -> Stack.pop s
      | _ -> ()) str;
    Stack.is_empty s
  with Stack.EmptyStackException -> false *)

(*  trasformare questo codice sopra in funtore *)

module type StackADT = sig
  type t
  exception EmptyStackException
  val empty : t
  val push: t -> char -> unit
  val pop: t -> unit
  val is_empty: t -> bool
end;;

module Matcher (Stack:StackADT) = struct

  let is_balanced str =
    let s = Stack.empty in try
      String.iter
      (fun c -> match c with
          '(' -> Stack.push s c
        | ')' -> Stack.pop s
        | _ -> ()) str;
      Stack.is_empty s
    with Stack.EmptyStackException -> false

end;; 


(* implementazione di uno stack con una lista *)
module StackImplList = struct
  type char_stack = {
    mutable c: char list
  }

  exception EmptyStackException;;

  let empty = { c = []};;

  let push stack ch = stack.c <- ch :: stack.c;;  

  let pop stack = 
    match stack.c with
      h::tl     -> stack.c <- tl
    | []        -> raise EmptyStackException;;

  let is_empty stack = stack.c = [];;

end;;

(* implementazione stack con un vettore *)
module StackImplVector = struct
  type char_stack = {
    mutable top : int;
    mutable arr : char array
  }

  exception EmptyStackException;;

  let empty = {top=0; arr = Array.make 10 ' '};;

  let push stack ch = stack.arr.(stack.top) <- ch; stack.top <- stack.top +1;;

  let pop stack = if stack.top = 0 then
                    raise EmptyStackException 
                  else 
                    stack.top <- stack.top -1;;

  let is_empty stack = stack.top = 0;;

end;;

module M0 = Matcher(StackImplList);;
module M1 = Matcher(StackImplVector);;

let prova = Printf.printf "%b\n"
(M0.is_balanced "a(b)+c(a+d(e-f))");
Printf.printf "%b\n"
(M0.is_balanced "a(b(+c(a+d(e-f))");
Printf.printf "%b\n"
(M1.is_balanced "a(b)+c(a+d(e-f))");
Printf.printf "%b\n"
(M1.is_balanced "a(b(+c(a+d(e-f))");;
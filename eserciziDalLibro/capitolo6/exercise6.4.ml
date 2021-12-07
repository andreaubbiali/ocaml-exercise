(* 
We can define a data type for simple arithmetic expressions as follows.

type unop = Neg
type binop = Add | Sub | Mul | Div
type exp =
      Constant of int
    | Unary of unop * exp
    | Binary of exp * binop * exp

Write a function eval : exp -> int to evaluate an expression, performing the calcu-
lation to produce an integer result.
*)

(* neg l'ho visto come un null *)
type unop = Neg;;
type binop = Add | Sub | Mul | Div;;
type exp =
      Constant of int
    | Unary of unop * exp
    | Binary of exp * binop * exp;;


let prova = Constant 5;;
let prova2 = Unary (Neg,Constant 6);;
let prova3 = Binary (Constant 4,Add,Constant 6);;

let calcolo_binop a b = function
    Add   -> a + b
  | Sub   -> a - b
  | Mul   -> a * b
  | Div   -> a / b;;

let rec calcolo_exp = function
    Constant a        -> a
  | Unary (_,a)       -> calcolo_exp a
  | Binary (a,b,c)    -> (calcolo_binop (calcolo_exp a)  (calcolo_exp c) b);;


let numero = Printf.printf "%d\n" (calcolo_exp prova);;
let numero = Printf.printf "%d\n" (calcolo_exp prova2);;
let numero = Printf.printf "%d\n" (calcolo_exp prova3);;
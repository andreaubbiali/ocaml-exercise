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

(* neg == - *)
type unop = Neg;; 
type binop = Add | Sub | Mul | Div;;
type exp =
      Constant of int
    | Unary of unop * exp
    | Binary of exp * binop * exp;;

let calc_binop num1 num2 = function 
    Add   -> num1 + num2
  | Sub   -> num1 - num2
  | Mul   -> num1 * num2
  | Div   -> num1 / num2 ;;

let rec eval = function
      Constant a            -> a
  |   Unary    (_,a)        -> - eval(a)
  |   Binary   (a,b,c)      -> calc_binop (eval a) (eval c) b ;;
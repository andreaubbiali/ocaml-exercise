(* 
Write the matrix datatype with the following operations:

  1.  A function zeroes to construct a matrix of size n×m filled with zeros..
  2.  A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
  3.  A function init to construct a square matrix (matrice quadrata) of a given size n filled with the first n×n integers.
  4.  A function transpose that transposes a generic matrix independently of its size and content.
  5.  The basics operators + and * that adds and multiplies two matrices non necessarily squared. 
*)

module Matrix = 
  struct

    type matrix = int list list;;

    let zeroes l n m =  List.init m (fun x -> (List.init n (fun x-> 0)));;

    let identity l = List.map (fun l -> List.nth l = 1) l;;



end;;
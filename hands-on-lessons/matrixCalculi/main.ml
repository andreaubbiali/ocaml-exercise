(* 
Write the matrix datatype with the following operations:

1.  A function zeroes to construct a matrix of size n×m filled with zeros..
2.  A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
3.  A function init to construct a square matrix (matrice quadrata) of a given size n filled with the first n×n integers.
4.  A function transpose that transposes a generic matrix independently of its size and content.
5.  The basics operators + and * that adds and multiplies two matrices non necessarily squared. 
*)

(* COMPILARE QUESTO MODULO CHE UTILIZZA UN FUNTORE:
      - ocamlc -c MatrixADT.mli
      - ocamlc -c Matrix.ml
      - ocamlc Matrix.cmo main.ml
      - ./a.out

*)

module TheMatrix = (Matrix.Matrix : MatrixADT.Matrix);;

open TheMatrix;;

let main() =
   let z = zeroes 5 2 and i = identity 7 in
   (Format.printf "%a\n" pp_print_matrix z) ;
   (Format.printf "%a\n" pp_print_matrix i) ;;
   (* and a = init 7 in 
     let c = a + i in
       let t = transpose c in 
         let p = t*i in 
           (Format.printf "%a\n" pp_print_matrix z) ; 
           (Format.printf "%a\n" pp_print_matrix i) ; 
           (Format.printf "%a\n" pp_print_matrix a) ; 
           (Format.printf "%a\n" pp_print_matrix c) ; 
           (Format.printf "%a\n" pp_print_matrix p) ; 
           (Format.printf "%a\n" pp_print_matrix t) ;; *)

main();;
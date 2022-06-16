open Matrix;;
open Operations;;

let m1 = Array.make_matrix 4 4 5;; 
let m2 = Array.make_matrix 4 4 0;; 

module Prova = Operations(Matrix);;

let (+) a b =  Prova.sum a b;;

let main() = Printf.printf "START \n";
            Matrix.printMatrix m1;
            Matrix.printMatrix m2;
            Matrix.printMatrix (m1 + m2);;

main();;
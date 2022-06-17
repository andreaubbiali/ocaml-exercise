open Matrix;;
open Operations;;

module Op = Operations(Matrix);;

let m1 = Array.make_matrix 4 4 5;; 
let m2 = Array.make_matrix 4 4 0;;

let main() = Op.exec m1 m2;;

main();;
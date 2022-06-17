module type OperationsADT =
    sig

        type t
        exception OperationException
        val equivalence: t -> t -> bool
        val copy: t -> t
        val sum: t -> t -> t
        val scalarMultiplication: t -> int -> t
        val multiplication: t -> t -> t
        val print: t -> unit

    end;;


module Operations(A: OperationsADT) = struct

    let (+) a b = A.sum a b;;

    let ( * ) a b = A.multiplication a b;;

    let ( = ) a b = A.equivalence a b;;

    let exec m1 m2 = Printf.printf "START \n";
            Printf.printf "Matrix 1 \n"; 
            A.print m1;
            Printf.printf "\nMatrix 2 \n";
            A.print m2;
            Printf.printf "\nSum \n";
            A.print (m1 + m2);
            Printf.printf "\nmultiplication \n";
            A.print (m1 * m2);
            Printf.printf "\nequivalent?(expected false) \n";
            Printf.printf "%b \n" (m1 = m2);
            Printf.printf "\nequivalent?(expected true) \n";
            Printf.printf "%b \n" (m1 = m1);
            Printf.printf "\ncopy of matrix1 \n";
            A.print (A.copy m1);
            Printf.printf "\nmultiplication matrix1 * 3 (expected 15) \n";
            A.print (A.scalarMultiplication m1 3);;

end;;
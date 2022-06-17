module type OperationsADT =
    sig

        type t
        exception OperationException
        val sum: t -> t -> t
        val multiplication: t -> t -> t
        val print: t -> unit

    end;;


module Operations(A: OperationsADT) = struct

    let (+) a b = A.sum a b;;

    let ( * ) a b = A.multiplication a b;;

    let exec m1 m2 = Printf.printf "START \n";
            Printf.printf "Matrix 1 \n"; 
            A.print m1;
            Printf.printf "\nMatrix 2 \n";
            A.print m2;
            Printf.printf "\nSum \n";
            A.print (m1 + m2);
            Printf.printf "\nmultiplication \n";
            A.print (m1 * m2);;

end;;
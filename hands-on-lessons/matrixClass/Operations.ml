module type OperationsADT =
    sig

        type t
        exception OperationException
        val sum: t -> t -> t
        val print: t -> unit

    end;;


module Operations(A: OperationsADT) = struct

    let (+) a b = A.sum a b;;

    let exec m1 m2 = Printf.printf "START \n";
            A.print m1;
            A.print m2;
            A.print (m1 + m2);;

end;;
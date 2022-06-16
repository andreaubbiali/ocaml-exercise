module type OperationsADT =
    sig

        type t
        exception OperationException
        val sum: t -> t -> t

    end;;


module Operations(A: OperationsADT) = struct

    let sum a b = A.sum a b;;

end;;
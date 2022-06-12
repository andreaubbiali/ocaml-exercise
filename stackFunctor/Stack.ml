module type StackInterface = 
    sig

        type primitive
        type t

        exception EmtpyException

        val empty: t
        val add: primitive -> t -> unit
        val removeFirstEl: t -> primitive
        val isEmpty: t -> bool
        val printa: t -> unit

    end;;

module Stack (X: StackInterface) = struct 
    open X;;

    let createEmptyStack = X.empty;;

    let push v lst = X.add v lst;;

    let pop lst = if X.isEmpty lst then
                    raise X.EmtpyException
                 else
                    X.removeFirstEl lst;;

    let printa lst = X.printa lst;;
    
end;;
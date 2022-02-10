module type FRMInterface = sig

    type primitive
    type t

    val empty : t

    val next : int -> t -> primitive

    val has_next : int -> t -> bool

    val add : t -> primitive -> t
    
end;;
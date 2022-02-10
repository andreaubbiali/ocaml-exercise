open FRMInterface;;

module FRM (Frm: FRMInterface) = struct
    open Frm;;

    let gfilter f el = 
        let rec filter res index = match (has_next index el) with
            true             -> if f (next index el) then
                                    filter (add res (next index el)) (index+1)
                                 else
                                    filter res (index+1)
        |   false            -> res
        in filter empty 0;;

end;;



module type FRMADT = sig

    type t

    val toLst: t list
    
end;;

module FRM (Frm: FRMADT) = struct
    open Frm;;

    let gfilter f = 
        let rec filter lst = function
             []                     -> lst
            | h::t when f(h)        -> filter (h::lst) t
            | h::t                  -> filter lst t
        in filter [] Frm.toLst;;

end;;



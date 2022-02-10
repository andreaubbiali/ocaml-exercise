module FRMArrayString = struct

    type primitive = string;;
    type t = string array;;

    let empty = Array.make 0 "";;

    let next index el = Array.get el index;;

    let has_next index el = index < Array.length el;;

    let add arr el = Array.append arr (Array.make 1 el);;

end;;
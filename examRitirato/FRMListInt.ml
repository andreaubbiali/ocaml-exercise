module FRMListInt = struct

    type primitive = int;;
    type t = primitive list;;

    let empty = [];;

    let next index lst = List.nth lst index;;

    let has_next index lst = index +1 < (List.length lst);;

    let add lst el = el::lst;;

end;;

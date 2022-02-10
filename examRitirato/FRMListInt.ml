module FRMListInt = struct

    type primitive = int;;
    type t = int list;;

    let empty = [];;

    let next index lst = List.nth lst index;;

    let has_next index lst = index < (List.length lst);;

    let add lst el = el::lst;;

end;;

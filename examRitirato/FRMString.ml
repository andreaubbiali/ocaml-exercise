module FRMString = struct

    type primitive = char;;
    type t = string;;

    let empty = "";;

    let next index el = String.get el index;;

    let has_next index el = index < String.length el;;
    
    let add str c = str ^ (Char.escaped c);;

end;;
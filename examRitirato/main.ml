open FRM;;
open FRMListInt;;

module Prova = FRM(FRMListInt);;

let pp = 1::3::5::4::6::2::[];;
let aa = Prova.gfilter (fun (y : FRMListInt.primitive) -> y < 5) pp;;

let rec printa_list = function
    []      -> ()
|   h::t  -> Printf.printf "%d\n" h; printa_list t;;

let main() = 
    Printf.printf "ciao\n";
    printa_list aa;;

main();;

(* PER COMPILARE
ocamlc FRMInterface.mli FRM.ml FRMListInt.ml
ocamlc FRM.cmo FRMListInt.cmo main.ml
*)
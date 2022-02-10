open FRM;;
open FRMListInt;;

module Prova = FRM(FRMListInt);;

let aa = Prova.gfilter (fun (y : FRMListInt.primitive) -> y < 5) [5::4];;

let main() = 
    Printf.printf "ciao\n";;

main();;

(*
ocamlc FRMInterface.mli FRM.ml FRMListInt.ml
ocamlc FRM.cmo FRMListInt.cmo main.ml
*)
open FRM;;
open FRMListInt;;
open FRMString;;
open FRMArrayString;;

module ListInt = FRM(FRMListInt);;
module FString = FRM(FRMString);;
module FAString = FRM(FRMArrayString);;

let lst = 1::3::5::4::6::2::[];;
let str = "ciao sonoa andrea";;
let astr = Array.make 5 "";;
Array.set astr 0 "ciao";;
Array.set astr 1 "come";;
Array.set astr 2 "stai";;
Array.set astr 3 "come";;
Array.set astr 4 "stai";;


let rec printa_int_list = function
    []      -> ()
|   h::t  -> Printf.printf "%d-" h; printa_int_list t;;

let print_string_array arr =
    let rec printa = function
        0       -> Printf.printf "%s-" (Array.get arr 0)
    |   n       -> Printf.printf "%s-" (Array.get arr n);
                    printa (n-1)
    in printa ((Array.length arr)-1);;

let main() = 
    printa_int_list (ListInt.gfilter (fun (y : FRMListInt.primitive) -> y < 5) lst);
    Printf.printf "\n";
    Printf.printf "%s\n" (FString.gfilter (fun (y : FRMString.primitive) -> y != 'a') str);
    print_string_array (FAString.gfilter (fun (y : FRMArrayString.primitive) -> String.equal y "come") astr);
    Printf.printf "\n";;

main();;

(* PER COMPILARE
ocamlc FRMInterface.mli FRM.ml FRMListInt.ml FRMString.ml FRMArrayString.ml
ocamlc FRM.cmo FRMListInt.cmo FRMString.cmo FRMArrayString.cmo main.ml
*)
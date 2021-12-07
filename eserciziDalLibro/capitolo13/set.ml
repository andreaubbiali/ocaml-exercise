(* FUNTORE *)
module type EqualSig = sig
  type t
  val equal : t -> t -> bool
end;;

(* MakeSet vuole come parametro un modulo che soddisfi la signature EqualSig *)
(* ArgName è il nome del modulo passato come argomento che utilizziamo dentro MakeSet *)
(* ALTRA PARTE DEL FUNTORE *)
module MakeSet (ArgName : EqualSig) = struct
  open ArgName
  type elt = ArgName.t
  type t = elt list
  let empty = []
  let mem x s = List.exists (equal x) s
  let add a b = a :: b
  let find x s = List.find (equal x) s
end;;

(* CASO SPECIFICO UTILIZZANDO UN FUNTORE *)
module StringCaseEqual = struct
  type t = string
  let equal s1 s2 =
  String.lowercase_ascii s1 = String.lowercase_ascii s2
end;;


module SSet = MakeSet (StringCaseEqual);;
let s = SSet.add "Great Expectations" SSet.empty;;

let prova = SSet.mem "great eXpectations" s;;
let prova2 = SSet.find "great eXpectations" s;;

let stampa = Printf.printf "%b\n" (prova);
             Printf.printf "%s\n" (prova2);;
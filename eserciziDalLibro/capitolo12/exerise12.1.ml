(* funziona solo perchè c'è un let x = "x" altrimenti non funzionerebbe *)
module A : sig
  val x : string
end = struct
  let x = 1
  let x = "x"
end;;

module B : sig
  val x : string
  val x : string
end = struct
  let x = "x"
end;;

(* secondo me non dovrebbe funzionare dato che g torna una stringa ma ad f serve un int *)
module M : sig
  val f : int -> int
  val g : string -> string
end = struct
  let g x = x
  let f x = g x
end


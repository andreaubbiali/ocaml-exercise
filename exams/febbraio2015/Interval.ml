open Comparable;;
open IntervalI;;

module Interval(Endpoint: Comparable): 
     (IntervalI with type endpoint = Endpoint.t) = 
  struct
    (* endpoint is something like the primitive type, used to compone an interval *)
    type endpoint = Endpoint.t
    (* is a tuple of endpoint . so if endpoint is int it will be (int, int) *)
    type interval = Interval of Endpoint.t * Endpoint.t | Empty
    exception WrongInterval

    let create low high =
      if Endpoint.compare low high > 0 then raise WrongInterval
      else if Endpoint.compare low high == 0 then Empty
      else Interval (low,high)

    let is_empty = function
      | Empty -> true
      | Interval _ -> false

    let contains t x =
      match t with
      | Empty -> false
      | Interval (l,h) ->
        Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

    let intersect t1 t2 =
      let min x y = if Endpoint.compare x y <= 0 then x else y in
      let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (l1,h1), Interval (l2,h2) ->
        create (max l1 l2) (min h1 h2)
    
    let tostring = function
    | Empty -> "[]"
    | Interval(l,h) -> "["^(Endpoint.tostring l)^", "^(Endpoint.tostring h)^"]" 
  end ;;

(*
There are 2 option to instantiate interval
- create the compare module and pass it into the functor (IntInterval)
- create the compare module directly creating the module (stringInterval)
*)
module IntCompare = struct

  type t = int 
  let compare = compare 
  let tostring = string_of_int

end;;
  
module IntInterval = Interval(IntCompare);;

module StringInterval = Interval(
  struct 
    type t = string 
    let compare = String.compare 
    let tostring = fun x -> x 
  end);;
(* Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur,
and Rømer (Look at:

http://en.wikipedia.org/wiki/Comparison_of_temperature_scales
to read about them).

1. Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
2. Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other
scales, note that the scale must be specified.

Hint. Define a proper datatype for the temperature. *)

(* esercizio 1 *)
type t_unit = Celsius|Fahrenheit|Kelvin|Rankine|Delisle|Newton|Reaumur|Romer ;;
type temperature = { value: float; tu: t_unit } ;;

let cons = [Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer] ;;
let cons_repr = [(Celsius, "C"); (Fahrenheit, "F"); (Kelvin, "K"); (Rankine, "R"); (Delisle, "De"); (Newton, "N"); (Reaumur, "Ré"); (Romer, "Rø")] ;;

let any2c t =
  match t.tu with
    Celsius    -> t
  | Fahrenheit -> { value = (t.value -. 32.) *. 5. /. 9.;    tu = Celsius }
  | Kelvin     -> { value = t.value -. 273.15 ;              tu = Celsius }
  | Rankine    -> { value = (t.value -. 491.67) *. 5. /. 9.; tu = Celsius }
  | Delisle    -> { value = 100. -. t.value *. 2. /. 3.;     tu = Celsius }
  | Newton     -> { value = t.value *. 100. /. 33.;          tu = Celsius }
  | Reaumur    -> { value = t.value *. 5. /. 4.;             tu = Celsius }
  | Romer      -> { value = (t.value -. 7.5) *. 40. /. 21.;  tu = Celsius } ;;

let c2any t u =
  match u with
    Celsius    -> t
  | Fahrenheit -> { value = t.value *. 9. /. 5. +. 32.;    tu = u }
  | Kelvin     -> { value = t.value +. 273.15;             tu = u }
  | Rankine    -> { value = t.value *. 9. /. 5. +. 491.67; tu = u }
  | Delisle    -> { value = (100. -. t.value) *. 3. /. 2.; tu = u }
  | Newton     -> { value = t.value *. 33. /. 100.;        tu = u }
  | Reaumur    -> { value = t.value *. 4. /. 5.;           tu = u }
  | Romer      -> { value = t.value *. 21. /. 40. +. 7.5;  tu = u } ;;

exception ValueError ;;

let temp2representations_string tunit = 
  let rec search tunit = function
      []      -> raise ValueError
  |   hd::tl   -> if fst(hd)==tunit then snd(hd) else search tunit tl
  in search tunit cons_repr;;


let prova unit = c2any {value = 42.; tu = Celsius} (fst(unit));;

let stamp temp = Printf.printf "%f   %s \n" temp.value (temp2representations_string temp.tu);;
let iter_temperature lst = List.iter stamp lst;;

let main() = let list_temperature = List.map prova cons_repr in
          Printf.printf "conversione del numero 42 celsius in\n";
          iter_temperature list_temperature;;



main();;
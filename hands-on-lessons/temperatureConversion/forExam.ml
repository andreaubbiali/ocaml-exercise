(* 
  Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur,
  and Rømer 
  
  (Look at: http://en.wikipedia.org/wiki/Comparison_of_temperature_scales to read about them).

  1. Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
  2. Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other
  scales, note that the scale must be specified.

  Hint. Define a proper datatype for the temperature. 
*)
type scale_type = Celsius|Fahrenheit|Kelvin|Rankine|Delisle|Newton|Reaumur|Romer ;;
type tempvalue = {number: float ;scale: scale_type};;

let scale_list = [Celsius;Fahrenheit;Kelvin;Rankine;Delisle;Newton;Reaumur;Romer];;

(* func to print *)
let type_to_string = function
    Kelvin                -> "Kelvin"
  | Celsius               -> "Celsius"
  | Fahrenheit            -> "Fahrenheit"
  | Rankine               -> "Rankine"
  | Delisle               -> "Delisle"
  | Newton                -> "Newton"
  | Reaumur               -> "Reaumur"
  | Romer                 -> "Romer";;

let printa_conversion value = Printf.printf "num: %10.2f    scale name: %s\n" value.number (type_to_string value.scale);;

let from_something_to_kelvin = function
    {number=num;scale=Kelvin}                 -> (Float.add num 0.0)
  | {number=num;scale=Celsius}                -> (Float.add num (-273.15))
  | {number=num;scale=Fahrenheit}             -> (Float.add num (-459.67))
  | {number=num;scale=Rankine}                -> (Float.add num 0.0)
  | {number=num;scale=Delisle}                -> (Float.add num 599.73)
  | {number=num;scale=Newton}                 -> (Float.add num (-90.14))
  | {number=num;scale=Reaumur}                -> (Float.add num (-218.52))
  | {number=num;scale=Romer}                  -> (Float.add num (-135.90));;

let from_kelvin_to_something num  = function
    Kelvin                              -> {number = (Float.add num 0.0); scale = Kelvin }
  | Celsius                             -> {number = (Float.add num (-273.15)); scale = Celsius }
  | Fahrenheit                          -> {number = (Float.add num (-459.67)); scale = Fahrenheit } 
  | Rankine                             -> {number = (Float.add num 0.0); scale = Rankine }
  | Delisle                             -> {number = (Float.add num 599.73); scale = Delisle }
  | Newton                              -> {number = (Float.add num (-90.14)); scale = Newton }
  | Reaumur                             -> {number = (Float.add num (-218.52)); scale = Reaumur }
  | Romer                               -> {number = (Float.add num (-135.90)); scale = Romer };;

let convert value = 
  from_kelvin_to_something (from_something_to_kelvin value) value.scale;;


let conversion_number num = 
  Printf.printf "Conversion of the number: %.2f\n" num;
  printa_conversion (convert {number=num; scale= Kelvin});
  printa_conversion (convert {number=num; scale= Celsius});
  printa_conversion (convert {number=num; scale= Fahrenheit});
  printa_conversion (convert {number=num; scale= Rankine});
  printa_conversion (convert {number=num; scale= Newton});
  printa_conversion (convert {number=num; scale= Reaumur});
  printa_conversion (convert {number=num; scale= Romer});;


let main() = conversion_number 5.0;;

main();;

(* APPUNTI non capisco bene la consegna cosa debba fare, in parte comunque è giusto *)
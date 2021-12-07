(* Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: beryllium (4),
magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then:
1.  Write a function that returns the highest atomic number in alkaline_earth_metals.
2.  Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest) da leggero a pesante.

Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54),
and radon (86). Then:

3.  Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number)
sorted in ascending order on the element names. *)

let alkaline_earth_metals = [("beryllium", 4); ("magnesium", 12); ("calcium", 20); ("strontium", 38); ("barium", 56); ("radium", 88)];;

(* creo una funzione per trovare tra due numeri il più alto *)
let isHighest a b = (a-b >= 0);;

let highest_atomic_number l= 
    let rec highest max = function
        []      -> max
    |   h::tl   -> if (isHighest (snd h) max) then highest (snd h) tl else highest max tl
    in highest 0 l;;

let isLightest a b = (a-b <= 0);;

(* ricerca del minimo in una lista. Ritorna un elemento della lista *)
let min_element_in_a_list l =
    let rec search_min min = function
        []      -> min
    |   h::tl   ->if (isLightest (snd h) (snd min)) then search_min h tl else search_min min tl
    in search_min (List.hd(l)) l;;


(* tolgo un dato elemento da una lista *)
let remove_element x l=
    let rec remove x newlist = function
        []      -> newlist
    |   h::tl   -> if (h==x) then remove x (newlist@tl) [] else remove x newlist tl
    in remove x [] l;;


(* sort in ascending order *)
let sort_ascending_order l =
    let rec order result = function
        []      -> result
    |   _       -> order (result@(min_element_in_a_list l)) 
    (remove_element (min_element_in_a_list l) l)
    in order [] l;;

(* funzione per printare la lista *)
let printa_lista l =
    let rec printa = function
        h::tl    -> Printf.printf " %s  %d" (fst(h)) (snd(h)); printa tl
    in printa l;;


let main() = Printf.printf "highest atomic number: %d\n" (highest_atomic_number alkaline_earth_metals);
             Printf.printf "min element of the list: %s %d\n" (fst (min_element_in_a_list alkaline_earth_metals)) (snd (min_element_in_a_list alkaline_earth_metals));
             Printf.printf "ordered list:\n" (printa_lista (sort_ascending_order alkaline_earth_metals));
             ;;


main();;
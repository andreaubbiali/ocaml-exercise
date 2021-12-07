(*
rifaccio l'esercizio guardando la documentazione
Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: beryllium (4),
magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then:
1.  Write a function that returns the highest atomic number in alkaline_earth_metals.
2.  Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest) da leggero a pesante.

Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54),
and radon (86). Then:

3.  Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number)
sorted in ascending order on the element names.
*)

let alkaline_earth_metals = [("beryllium", 4); ("magnesium", 12); ("calcium", 20); ("strontium", 38); ("barium", 56); ("radium", 88)];;

(* esercizio 1 TROVA L'HIGHEST *)
let is_highest a b = if ((snd a)-(snd b) >= 0) then a else b;;

let highest_atomic lst = List.fold_left is_highest (List.hd lst) (List.tl lst);;

(* esercizio 2 SORT THA LISTA *)
let diff a b = snd(a)-snd(b);;
let sort_list lst = List.sort diff lst;;

(* splitto la lista *)
let split_list lst = List.split lst;;

let printDouble a b = Printf.printf "%s  %d\n" a b;;

(* printa una lista *)
let print_double_list lst1 lst2 = List.iter2 printDouble lst1 lst2;;

(* esercizio 3 MERGE AND PRINT LIST *)
let noble_gases = [("helium", 2); ("neon", 10); ("argon", 18); ("krypton", 36); ("xenon", 54); ("radon", 86)];;
let merge_list lst1 lst2 = List.merge diff lst1 lst2;;


let main() = let sorted_list = sort_list alkaline_earth_metals in
             let splitted_list = split_list sorted_list in
             let splitted_merged_list = split_list (merge_list alkaline_earth_metals noble_gases) in
             Printf.printf "highest atomic number: %d\n" (snd(highest_atomic alkaline_earth_metals));
             Printf.printf "printa lista: \n" ;
             print_double_list (fst(splitted_list)) (snd(splitted_list));
             Printf.printf "printa lista mergiata: \n";
             print_double_list (fst(splitted_merged_list)) (snd(splitted_merged_list));
            ;;

main();;
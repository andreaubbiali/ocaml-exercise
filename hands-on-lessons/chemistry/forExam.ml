(* Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: 
beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then

  Write a function that returns the highest atomic number in alkaline_earth_metals.
  Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).

Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). Then

  Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number) 
  sorted in ascending order on the element names. 

*)

let alkaline_earth_metals = ("magnesium", 12)::("beryllium", 4)::("calcium", 20)::("strontium", 38)::("barium", 56)::("radium", 88)::[];;

let noble_gases = [("helium", 2); ("neon", 10); ("argon", 18); ("krypton", 36); ("xenon", 54); ("radon", 86)] ;;

let max = 0;;

let check_max (_,a) = a > max;;

let get_max l = List.find check_max l;;

let compare (_,a) (_,b) = if a == b then
                            0
                          else if a > b then
                            1
                          else
                            -1;;

let sort_ascending l = List.sort compare l;;

let merge_and_order_list l1 l2 = List.merge compare (sort_ascending l1) (sort_ascending l2);;

let print_list l = 
  let rec printa = function
    (name, number)::tl       -> Printf.printf "name:   %s   number:   %d\n" name number; printa tl
  | []                       ->()
  in printa l;;

let main() =
  let m = (get_max alkaline_earth_metals) and
      sorted_aem = sort_ascending alkaline_earth_metals in
    Printf.printf "The heaviest element is «%s» with «%2d» as atomic number.\n" (fst m) (snd m) ;
    Printf.printf "\n";
    print_list sorted_aem ;
    Printf.printf "\n";
    print_list (merge_and_order_list alkaline_earth_metals noble_gases) ;;

main() ;;
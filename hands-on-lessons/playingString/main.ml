(* 
Define the following functions/operators on strings:

  1-is_palindrome: string → bool that checks if the string is palindrome, a string is palindrome when the represented sentence can be read the same
      way in either directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...
  2-operator (-): string → string → string that subtracts the letters in a string from the letters in another string, 
      e.g., "Walter Cazzola"-"abcwxyz" will give "Wlter Col" note that the operator - is case sensitive
  3-anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is 
      an anagram of one or more of the strings in the dictionary

*)

(* per integer *)
let (--) a b = a-b;;

(* EXERCISE 2 *)
let (-) word letters = 
  let len = String.length letters in
  let rec check str i =
    match i with
      -1     -> str
    | _     -> check (String.concat "" (String.split_on_char (String.get letters i) str)) (i-1)
  in check word (len-1);;



(* EXERCISE 1 *)
let is_palindrome word =
  let s = (String.trim (String.lowercase_ascii (word-" .,"))) and
  len = (String.length (String.trim (word-" .,")))--1 in
  let rec is_equal i=
    match i with
      i when i > len/2    -> true
    | _                   -> if (String.get s i) != (String.get s (len--i)) then false else is_equal (i+1)
  in is_equal 0;;


(* EXERCISE 3 *)
let dictionary = ["vile"; "elegant man"; "twelve plus one"; ""];;

let is_anagram a b =
  if (String.length a) != (String.length b) then 
    false
  else
    if (a-b) = "" then
      true
    else
      false;;


let anagram word = 
  let w = String.lowercase_ascii (word-" ") in
  let rec check = function 
      h::tl     -> if is_anagram w (String.lowercase_ascii (h-" ")) then true else check tl
    | []        -> false
  in check dictionary;;


let main()=
  Printf.printf "true:  %b\n" (is_palindrome "detartrated");
  Printf.printf "false:  %b\n" (is_palindrome "andrea");
  Printf.printf "true:  %b\n" (is_palindrome "Do geese see God");
  Printf.printf "true:  %b\n" (is_palindrome "Rise to vote, sir.");
  Printf.printf "true:  %b\n" ("Wlter Col" = ("Walter Cazzola"-"abcwxyz"));
  Printf.printf "true:  %b\n" (anagram "evil");
  Printf.printf "true:  %b\n" (anagram "vile");
  Printf.printf "true:  %b\n" (anagram "a gentleman");
  Printf.printf "true:  %b\n" (anagram "eleven plus two");
  Printf.printf "false:  %b\n" (anagram "andrea");
  ;;

main();;

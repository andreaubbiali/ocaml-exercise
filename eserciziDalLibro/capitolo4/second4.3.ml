(* 
Suppose we have a crypto-system based on the following substitution
cipher, where each plain letter is encrypted according to the following table.

Plain           A   B   C   D
Encrypted       C   A   D   B

For example, the string BAD would be encrypted as ACB .
Write a function check that, given a plaintext string s1 and a ciphertext string s2 ,
returns true if, and only if, s2 is the ciphertext for s1 . Your function should raise an
exception if s1 is not a plaintext string. You may wish to refer to the string operations
on page 8. How does your code scale as the alphabet gets larger?
*)

let couples = [('A', 'C'); ('B', 'A'); ('C', 'D'); ('D', 'B')];;

(* return true if are of the same size false otherwise *)
let check_size s1 s2 = (String.length s1) == (String.length s2);;

(* return the value of the key *)
let get_value key =
  let rec get_value = function
      h::tl   -> if (snd h) == key then fst h else get_value tl
    | []      -> raise(Invalid_argument "Letter not in the couples")
  in get_value couples;;

(* return true if a is the encryption of b false otherwise *)
let is_right_encryption a b = (get_value a) == b ;;

(* check if s2 is the ciphertext for s1*)
let check s1 s2 = 
  if (check_size s1 s2) == false then 
    raise(Invalid_argument "String not equal size")
  else 
    (* iterate on each character *)
      let rec check_equals l = 
        if l == 0  || (String.length s1) == 0 then 
          true
        else
          if (is_right_encryption s1.[l] s2.[l]) == false then
            false
          else
            check_equals (l-1)
    in check_equals ((String.length s1)-1);;

let main() = Printf.printf "Giusta cifratura:    %b\n" (check "ACB" "BAD");
             Printf.printf "Sbagliata cifrat:    %b\n" (check "BDC" "BAD");
             Printf.printf "Lunghezza  0:        %b\n" (check "" "");
             Printf.printf "letttere not exist:  %b\n" (check "BAS" "ACC");
             Printf.printf "Lunghezza diversa:   %b\n" (check "assda" "bb");; 
             

main();;
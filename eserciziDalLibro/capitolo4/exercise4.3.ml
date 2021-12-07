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

(* s1=str_plain; s2=str_cipher *)

let check s1 s2 =
  if String.length s1 != String.length s2 then false 
  else 
    let len = String.length s1 in
      let rec control_letter = function
            i when i = len                 -> true
        (* |   i when i < len && s1.[i] = 'A' -> (Printf.printf "numero:   %d\n" i); if s2.[i] = 'C' then control_letter (i+1) else false *)
        |   i when i < len && s1.[i] = 'A' -> if s2.[i] = 'C' then control_letter (i+1) else false
        |   i when i < len && s1.[i] = 'B' -> if s2.[i] = 'A' then control_letter (i+1) else false
        |   i when i < len && s1.[i] = 'C' -> if s2.[i] = 'D' then control_letter (i+1) else false
        |   i when i < len && s1.[i] = 'D' -> if s2.[i] = 'B' then control_letter (i+1) else false
        |   i                              -> raise(Invalid_argument "error in function check! its ok")
        in control_letter 0;
;;

  
(* MIGLIORAMENTO SE DOVESSE ALLARGARSI L'ALFABETO *)
let couple_list = [('A', 'C'); ('B', 'A'); ('C', 'D'); ('D', 'B')];;

let matchano a b = 
  let rec ricerca = function
      h::tl       -> if (fst h) != a then ricerca tl else if (snd h) = b then true else false
    | []          -> raise(Invalid_argument "error in function matchano. L'elemento ricercato non c'è nella lista")
  in ricerca couple_list;;

let check_better s1 s2 =
  if String.length s1 = 0 || String.length s2 = 0 then false 
  else if String.length s1 != String.length s2 then false
  else
    let len = String.length s1 in
      let rec control_letter = function
            i when i = len                 -> true
        |   i when i < len                 -> if (matchano s1.[i] s2.[i]) then control_letter (i+1) else false
        |   i                              -> raise(Invalid_argument "error in function check_better! STRANO NON DOVREBBE MAI USCIRE")
        in control_letter 0;
;;

let main() = Printf.printf "Giusta cifratura:    %b\n" (check_better "BAD" "ACB");
             Printf.printf "Lunghezza diversa:   %b\n" (check_better "assda" "bb");
             Printf.printf "Sbagliata cifrat:    %b\n" (check_better "BAD" "BDC");
             Printf.printf "Lunghezza  0:        %b\n" (check_better "" "");
             Printf.printf "letttere not exist:  %b\n" (check_better "BAS" "ACC");; (* crea eccezione*)

main();;

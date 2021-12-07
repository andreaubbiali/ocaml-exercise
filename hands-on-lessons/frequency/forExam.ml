(* 
Let's write a function (or a pool of functions) that given a quite large text (over than 2000 words) counts how frequent each word occurs in the text.
The text is read from a file (look at the pervasive module in the manual) and it is a real text with punctuation (i.e., commas, semicolons, ...) that should be counted.
Note that words with different case should be considered the same.
*)

let file_path = "prova.txt";;

let number_char = 255;;

(* print list *)

let rec printa_lista = function
    (c, num)::tl               -> if num != 0 then Printf.printf "%c      %d\n" (Char.chr c) num; printa_lista tl
  | []                         -> ();;

(* count frequencies *)

let count_frequency ch txt = 
  let text = String.lowercase_ascii txt and
  cont = String.length txt in
  let rec count num i =
    match i with
      0           -> num
    | _           -> if (String.get text i) = ch then 
                      count (num+1) (i-1) 
                    else 
                      count num (i-1) 
  in count 0 (cont-1);;

(* from 255 to 0 count the frequency *)
let check_frequency txt = 
  let rec count_char lst i = 
    match i with
      -1            -> lst
    | _             -> count_char ([(i, count_frequency (Char.chr i) txt)]@lst) (i-1)
  in count_char [(1,0)] number_char;;


(* read lines and put in a unique string *)
let read_file filename = 
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan ^ !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    !lines ;;

let readed = read_file file_path;;


let main() = 
  printa_lista (check_frequency readed)
;;

main();;
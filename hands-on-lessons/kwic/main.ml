(* 
A KeWord In Context (KWIC) index is a simple index for a list of lines or titles. This assignment involves creating a KWIC index 
for an input list of titles stored in a file. Here's a small example. For the input file:

  Casablanca
  The Maltese Falcon
  The Big Sleep

your program should produce the output:

  3                              The Big Sleep    .
  1                                  Casablanca   .
  2                      The Maltese Falcon       .
  2                              The Maltese Falcon
  3                          The Big Sleep        .

As you can see, each title is listed for each word (omitting some minor words). The titles are arranged so that the word being indexed is shown in a column on the page.
The position the lines have in the input file is shown on the left in the result.

Your solution should follow the following rules:


  - The input is just a series of titles, one per line. Any leading or trailing spaces should be removed. Internal spaces should be retained (trimmed to one).
  - A word is a maximal sequence of non-blank characters.
  - The output line is at most 79 characters wide.
      ->The number is 5 characters wide, right-justified.
      ->There is a space after the number.
      ->The key word starts at position 40 (numbering from 1).
      ->If the part of the title left of the keyword is longer than 33, trim it (on the left) to 33.
      ->If the part of the keyword and the part to the right is longer than 40, trim it to 40.
  - Each title appears in the output once for each word that isn't minor. Any word of length two or less is minor, and the words are minor words.
  - If a title has a repeated word, it should be listed for each repetition.
  - Sorting should be case-insensitive.
*)

let file_name = "file.txt";;

(* read the file *)
let read_file f =
  let l = ref [] and
  cont = ref 1 and
  c = open_in f in
  try
   while true; do
    l := (!cont, (String.trim(input_line c)))::!l;
    cont := !cont+1
   done;!l
  with End_of_file ->
    !l;;



(* something is a word if length > 3 ("the" is not a word in the example) *)
let is_a_word w = if (String.length w) > 2 && (String.equal w "The") == false && (String.equal w "the") == false then true else false;;

(* return the number of words in a list *)
let count_words_number lst = 
  let rec check cont = function
      h::tl       -> if is_a_word h then check (cont+1) tl else check cont tl
    | []          -> cont
  in check 0 lst;;

(* return a list with the line repeated num times*)
let repeat_line_in_list num_line line rep = 
  let lst = ref [] in
  let rec create i = 
    match i with
      0       -> !lst
    | _       -> lst := (num_line, line)::!lst; create (i-1)
  in create rep;;

(* create the output list *)
let create_list_based_on_word l =
  let rec create lst = function
      (num, line)::tl               -> create (( repeat_line_in_list num line (count_words_number (String.split_on_char ' ' line)))@lst) tl
    | []                            -> lst
  in create [] l;;
  

let file_lines = read_file file_name;;
let output_list = create_list_based_on_word file_lines;;


(* print a list *)
let rec printa = function
    (num, str)::tl       -> Printf.printf "%5d %40s\n" num str; printa tl
  | []                   -> ();;

let main() = printa output_list;
;;

main();;
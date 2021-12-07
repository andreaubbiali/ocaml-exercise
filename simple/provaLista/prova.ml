(* TUTTI QUESTI ESERCIZI in realtà ci può aiutare la libreria List ma li facciamo per esempio *)

let list1 = [5; 4; 8; 6; 10];;
let list2 = [1; 4; 6; 9; 15];;

let ll = list1@list2;;

(* cercare un valore nella lista *)
let rec find l x = if l==[] then false else x==List.hd(l) || find (List.tl l) x;;

(* contare numero di occorrenze *)
let count l x =
    (* l non scritta perchè sottintesa per il matching *)
    let rec count tot x = function
        []      -> tot
    |   h ::tl  -> if (h==x) then count (tot+1) x tl else count tot x tl
    in count 0 x l;;


let listLetter = ['a'; 'b'; 'c'; 'a'; 'c'; 'a'];;

let countOccurrenceLetter l lecter =
    let rec countOccurrence tot lecter = function
        []          -> tot
    |   head::tl    -> if (head==lecter) then countOccurrence (tot+1) lecter tl else countOccurrence tot lecter tl
    in countOccurrence 0 lecter l;;



(* posizione di un elemento nella lista *)
let posizioneElemento l elemento = 
    let rec posizione l elemento posiz= 
        if (List.hd l)==elemento then posiz else posizione (List.tl l) elemento (posiz+1)
    in posizione l elemento 0;;

let posizioneElemento1 l elemento =
    let rec posizione elemento posiz = function
        []                          -> -1
    |   head::tl                    -> if (head==elemento) then posiz else posizione elemento (posiz+1) tl
    in posizione elemento 0 l;;


(* posso chiamare sia countOccurrence che count sia che sono lettere che numeri perchè sono generiche *)
let main() = Printf.printf "C'è il valore %d nella lista: %b
Numero di occorrenze del valore %d:  %d
Numero di occorrenze del valore %s:  %d
Chiamata alla vecchia funzione count. lettera %s:  %d
Posizione elemento %s:  %d
Posizione elemento %d:  %d
prova con l'altra funzione perchè quella sopra non va bene se non esiste l'elemento:
Posizione elemento %s:  %d
Posizione elemento che non c'è %s:  %d
Posizione elemento %d:  %d\n" 
1 (find ll 1) 
6 (count ll 6) 
"a" (countOccurrenceLetter listLetter 'a') 
"c" (count listLetter 'c')
"b" (posizioneElemento listLetter 'b')
10 (posizioneElemento ll 10)
"b" (posizioneElemento1 listLetter 'b')
"f" (posizioneElemento1 listLetter 'f')
10 (posizioneElemento1 ll 10)
;;

main();;
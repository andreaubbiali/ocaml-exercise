open Stack;;
open StackList;;


module Prova = Stack(StackList);;

let main() = let stc = Prova.createEmptyStack in
            Prova.push "prova" stc;
            Prova.push "second" stc;
            Prova.printa stc;
            Printf.printf "popped: %s\n" (Prova.pop stc);
            Prova.printa stc;
            Printf.printf "popped: %s\n" (Prova.pop stc);
            Prova.printa stc;
            Printf.printf "popped: %s\n" (Prova.pop stc);;
main();;
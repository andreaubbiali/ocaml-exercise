
let prova x = x + 1;;

let compose f g = f (g);;

let result0 = prova 6;;

let result = compose result0 prova;;

let main() = Printf.printf "result: %d \n" (result);;

(* let compose f g x = f (g x);;

let compose1 f g x = (f g x);; *)


main();;
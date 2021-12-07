
let prova x = x + 1;;

let compose0 f g = f + g;;

let result0 = prova 6;;

let result = compose0 5 result0 ;;

let main() = Printf.printf "result: %d \n" (result);;


main();;